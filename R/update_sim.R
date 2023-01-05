#' Update a simulation
#'
#' @description This function updates a previously run simulation. After a
#'    simulation has been \code{\link{run}}, you can alter the levels of the
#'    resulting object of class \code{sim_obj} using \code{\link{set_levels}},
#'    or change the configuration (including the number of simulation
#'    replicates) using \code{\link{set_config}}. Executing \code{update_sim} on
#'    this simulation object will only run the added levels/replicates, without
#'    repeating anything that has already been run.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}, that has already been run by the \code{\link{run}}
#'     function
#' @param keep_errors logical (\code{TRUE} by default); if \code{TRUE}, do not
#'     try to re-run simulation reps that results in errors previously; if
#'     \code{FALSE}, attempt to run those reps again
#' @details \itemize{
#'   \item{It is not possible to add new level variables, only new levels of the
#'   existing variables. Because of this, it is best practice to include all
#'   potential level variables before initially running a simulation, even if
#'   some of them only contain a single level. This way, additional levels can
#'   be added later.}
#' }
#' @return The original simulation object with additional simulation replicates
#'     in \code{results} or \code{errors}
#' @examples
#' sim <- new_sim()
#' create_data <- function(n) { rpois(n, lambda=5) }
#' est_mean <- function(dat, type) {
#'   if (type=="M") { return(mean(dat)) }
#'   if (type=="V") { return(var(dat)) }
#' }
#' sim %<>% set_levels(n=c(10,100), est="M")
#' sim %<>% set_config(num_sim=10)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- est_mean(dat=dat, type=L$est)
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
#' sim %<>% set_config(num_sim=5)
#' sim %<>% update_sim()
#' @importFrom magrittr %>%
#' @export
update_sim <- function(sim, keep_errors=T) {
  UseMethod("update_sim")
}

#' @export
update_sim.sim_obj <- function(sim, keep_errors=T) {

  # Error handling
  if (sim$vars$run_state == "pre run") {
    stop("Simulation has not been run yet.")
  }
  handle_errors(keep_errors, "is.boolean")

  # Add "global" objects to simulation object run environment (excluding
  #     simulation object); these are not necessarily in the global environment,
  #     but are in the environment the new_sim() call is executed within.
  for (obj_name in ls(sim$internals$env_calling, all.names=T)) {
    obj <- get(x=obj_name, envir=sim$internals$env_calling)
    if (!methods::is(obj,"sim_obj") && obj_name!="L") {
      assign(x=obj_name, value=obj, envir=sim$vars$env)
    }
  }

  # Make sorted list of current levels and previous levels
  sorted_prev_levels <- sim$internals$levels_prev[
    order(names(sim$internals$levels_prev))]
  sorted_curr_levels <- sim$internals$levels_shallow[
    order(names(sim$internals$levels_shallow))]

  # Disallow adding new level variables
  if (length(names(sorted_prev_levels))!=length(names(sorted_curr_levels)) ||
      !all.equal(names(sorted_prev_levels),names(sorted_curr_levels))) {
    stop("Updating a sim cannot include new level variables, only new levels.")
  }

  # Make grid of previously run levels
  prev_levels_grid_big <- sim$internals$levels_grid_big
  i1 <- which(names(prev_levels_grid_big) %in% c(
    "sim_uid", "rep_id", "batch_id", "core_id"
  ))
  prev_levels_grid <- prev_levels_grid_big[,-i1, drop=F] %>%
    dplyr::distinct()

  if (!("no levels" %in% names(sorted_prev_levels))) {

    # Create level_ids for new levels; add to sim$levels_grid
    sim$levels_grid <- dplyr::left_join(
      sim$levels_grid[,-which(names(sim$levels_grid)=="level_id"), drop=F],
      prev_levels_grid,
      by = names(sorted_prev_levels)
    )
    if (sum(is.na(sim$levels_grid$level_id)) > 0) {
      max_level_id <- max(prev_levels_grid$level_id)
      num_new_levels <- sum(is.na(sim$levels_grid$level_id))
      new_level_ids <- (max_level_id+1):(max_level_id+num_new_levels)
      sim$levels_grid$level_id[is.na(sim$levels_grid$level_id)] <- new_level_ids
    }

    # Reorder columns
    sim$levels_grid <- cbind(level_id=sim$levels_grid$level_id,
                             sim$levels_grid[,-length(sim$levels_grid), drop=F])
  }

  # Create levels_grid_big
  levels_grid_big <- create_levels_grid_big(sim, update=T)

  # Create new sim_uids; add to levels_grid_big
  i2 <- which(names(levels_grid_big)=="sim_uid")
  i3 <- which(names(prev_levels_grid_big) %in% c("sim_uid", "level_id",
                                                 "rep_id"))
  levels_grid_big <- dplyr::left_join(
    levels_grid_big[,-i2, drop=F],
    prev_levels_grid_big[,i3, drop=F],
    by = c("level_id", "rep_id")
  )
  if (sum(is.na(levels_grid_big$sim_uid)) > 0) {
    max_uid <- max(prev_levels_grid_big$sim_uid)
    num_new_uids <- sum(is.na(levels_grid_big$sim_uid))
    new_uids <- (max_uid+1):(max_uid+num_new_uids)
    levels_grid_big$sim_uid[is.na(levels_grid_big$sim_uid)] <- new_uids
  }

  # Reorder columns
  levels_grid_big <- cbind(sim_uid=levels_grid_big$sim_uid,
                           levels_grid_big[,-length(levels_grid_big)])

  # If re-running error reps, limit the prev_levels_grid to only those in
  #     results and revert the error df
  if (!keep_errors) {
    # !!!!! return to this
    prev_levels_grid_big <- dplyr::semi_join(prev_levels_grid_big,
                                             sim$results,
                                             by="sim_uid")
    sim$errors <- "No errors"
  }

  # Get levels / sim_uids that have not previously been run
  uids_to_run <- setdiff(levels_grid_big$sim_uid,
                          prev_levels_grid_big$sim_uid)

  # Get levels / sim_uids that were previously run but are no longer needed
  uids_to_delete <- setdiff(prev_levels_grid_big$sim_uid,
                           levels_grid_big$sim_uid)

  if (length(uids_to_run)==0 && length(uids_to_delete)==0) {
    warning("There were no simulation replicates to run or remove.")
  }

  if (Sys.getenv("sim_run")!="") {

    # If on cluster, delete old results/errors/warnings
    sim$results <- NULL
    # !!!!! results_complex
    sim$errors <- NULL
    sim$warnings <- NULL

  } else {

    # Remove results/errors/warnings marked for deletion
    if (length(uids_to_delete)>0) {

      if (!is.character(sim$results)) {
        sim$results %<>% dplyr::filter(!(sim_uid %in% uids_to_delete))
      }
      if (!is.character(sim$errors)) {
        sim$errors %<>% dplyr::filter(!(sim_uid %in% uids_to_delete))
      }
      if (!is.character(sim$warnings)) {
        sim$warnings %<>% dplyr::filter(!(sim_uid %in% uids_to_delete))
      }
      # !!!!! remove results_complex too

    }

  }

  # Set update flags
  sim$internals$update_sim <- TRUE
  assign(x="..flag_batch_update", value=T, envir=sim$vars$env)

  if (length(uids_to_run)>0) {

    # Create a copy of the sim to hold new results
    sim_copy <- sim

    # Run sim_uids that have not been run yet
    sim_copy$internals$levels_grid_big <- dplyr::filter(
      levels_grid_big, sim_uid %in% uids_to_run
    )
    sim_copy$results <- NULL
    sim_copy$errors <- NULL
    sim_copy$warnings <- NULL
    sim_copy %<>% run()

    # Attach actual levels_grid_big
    sim$internals$levels_grid_big <- levels_grid_big

    # Combine results/errors/warnings of original run and updated run
    if (sim_copy$vars$run_state %in% c("run, no errors", "run, some errors")) {
      if (!is.character(sim$results)) {
        sim$results <- rbind(sim$results, sim_copy$results)
      } else {
        sim$results <- sim_copy$results
      }
    }
    if (sim_copy$vars$run_state %in% c("run, all errors", "run, some errors")) {
      if (!is.character(sim$errors)) {
        sim$errors <- rbind(sim$errors, sim_copy$errors)
      } else {
        sim$errors <- sim_copy$errors
      }
    }
    if (!is.character(sim_copy$warnings)) {
      if (!is.character(sim$warnings)) {
        sim$warnings <- rbind(sim$warnings, sim_copy$warnings)
      } else {
        sim$warnings <- sim_copy$warnings
      }
    }

  }

  return (sim)

}
