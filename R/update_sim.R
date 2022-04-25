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
#' @param keep_extra logical (\code{FALSE} by default); if \code{TRUE}, keep
#'     previously run simulation reps even if they exceed the current
#'     \code{num_sim} in config or are from a level that has been dropped; if
#'     \code{FALSE}, drop excess reps (starting from the last rep for that
#'     particular simulation level)
#' @details \itemize{
#'   \item{It is not possible to add new level variables, only new levels of the
#'   existing variables. Because of this, it is best practice to include all
#'   potential level variables before initially running a simulation, even if
#'   some of them only contain a single level. This way, additional levels can
#'   be added later.}
#'   \item {In general, if \code{num_sim} has been reduced prior to running
#'   \code{update_sim}, it is best to use the default option
#'   \code{keep_extra = FALSE}. Otherwise, some simulation levels will have more
#'   replicates than others, which makes comparison difficult.}
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
update_sim <- function(sim, keep_errors=TRUE, keep_extra=FALSE) {
  UseMethod("update_sim")
}

#' @export
update_sim.sim_obj <- function(sim, keep_errors=TRUE, keep_extra=FALSE) {

  handle_errors(sim, "is.sim_obj")

  # Error if simulation has not yet been run
  if (sim$vars$run_state == "pre run") {
    stop("Simulation has not been run yet.")
  }

  # error handle invalid options
  handle_errors(keep_errors, "is.boolean")
  handle_errors(keep_extra, "is.boolean")

  # Add "global" objects to simulation object run environment (excluding
  #     simulation object); these are not necessarily in the global environment,
  #     but are in the environment the new_sim() call is executed within.
  for (obj_name in ls(sim$internals$env_calling)) {
    obj <- get(x=obj_name, envir=sim$internals$env_calling)
    if (!methods::is(obj,"sim_obj") && obj_name!="L") {
      assign(x=obj_name, value=obj, envir=sim$vars$env)
    }
  }

  # make sorted list of current levels and previous levels
  sorted_prev_levels <- sim$internals$levels_prev[
    order(names(sim$internals$levels_prev))]
  sorted_curr_levels <- sim$internals$levels_shallow[
    order(names(sim$internals$levels_shallow))]

  # disallow adding new level variables
  if (length(names(sorted_prev_levels))!=length(names(sorted_curr_levels)) ||
      !all.equal(names(sorted_prev_levels),names(sorted_curr_levels))) {
    stop("Updating a sim cannot include new level variables, only new levels.")
  }

  # make grid of previously run levels / sim_ids
  prev_levels_grid_big <- sim$internals$levels_grid_big
  prev_levels_grid <- prev_levels_grid_big[,-c(1,3), drop = FALSE] %>%
    dplyr::distinct()
  if (!("no levels" %in% names(sorted_prev_levels))) {
    sim$levels_grid <- dplyr::left_join(
      sim$levels_grid[,-which(names(sim$levels_grid)=="level_id"), drop=FALSE],
      prev_levels_grid,
      by = names(sorted_prev_levels)
    )
    max_levelid <- max(prev_levels_grid$level_id)
    if (sum(is.na(sim$levels_grid$level_id)) > 0) {
      new_levelids <- (max_levelid + 1):(
        max_levelid + sum(is.na(sim$levels_grid$level_id))
      )
      sim$levels_grid$level_id[is.na(sim$levels_grid$level_id)] <- new_levelids
    }
  }

  # Create levels_grid_big
  levels_grid_big <- create_levels_grid_big(sim)
  levels_grid_big <- dplyr::left_join(
    levels_grid_big[,-which(names(levels_grid_big)=="sim_uid"), drop=FALSE],
    prev_levels_grid_big,
    by = names(levels_grid_big)[-which(names(levels_grid_big)=="sim_uid")])
  # !!!!! use the internal total_sim here instead. this is currently not quite correct
  max_uid <- sim$internals$num_sim_cuml
  if (sum(is.na(levels_grid_big$sim_uid)) > 0) {
    new_uids <- (max_uid + 1):(max_uid + sum(is.na(levels_grid_big$sim_uid)))
    levels_grid_big$sim_uid[is.na(levels_grid_big$sim_uid)] <- new_uids
  }

  col_order <- c("sim_uid", "level_id", "sim_id",
                 names(sim$internals$levels_shallow)[
                   !(names(sim$internals$levels_shallow) == "no levels")
                 ])
  levels_grid_big <- levels_grid_big[,col_order]

  # if re-running error reps, limit the prev_levels_grid to only those in
  #     results and revert the error df
  if (!keep_errors) {
    prev_levels_grid_big <- dplyr::semi_join(prev_levels_grid_big,
                                         sim$results,
                                         by = names(prev_levels_grid_big))
    sim$errors <- "No errors"
  }

  # get levels / sim_ids that have not previously been run
  not_run <- dplyr::anti_join(
    levels_grid_big,
    prev_levels_grid_big,
    by = names(prev_levels_grid_big[
      ,-which(names(prev_levels_grid_big) == "sim_uid")
    ])
  )

  # get levels / sim_ids that were previously run but are no longer needed
  extra_run <- dplyr::anti_join(
    prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in%
                                   c("sim_uid", "level_id")),drop=F],
    levels_grid_big[,-which(names(levels_grid_big) %in%
                              c("sim_uid", "level_id")),drop=F],
    by = names(prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in%
                                              c("sim_uid","level_id")),drop=F]))

  # if keep_extra = FALSE, remove excess runs (from results, errors, and
  #     warnings)
  if (!keep_extra & nrow(extra_run) > 0) {

    if (!is.character(sim$results)) {
      sim$results <- dplyr::anti_join(sim$results,
                                          extra_run,
                                          by = names(extra_run))
    }
    if (!is.character(sim$errors)) {
      sim$errors <- dplyr::anti_join(sim$errors,
                                         extra_run,
                                         by = names(extra_run))
    }
    if (!is.character(sim$warnings)) {
      sim$warnings <- dplyr::anti_join(sim$warnings,
                                           extra_run,
                                           by = names(extra_run))
    }
  }

  # if on cluster, delete old results, errors, etc
  if (Sys.getenv("sim_run")!="") {
    sim$results <- NULL
    sim$errors <- NULL
    sim$warnings <- NULL
  }
  # create a copy of the sim to hold new results
  sim_copy <- sim
  sim_copy$internals$update_sim <- TRUE
  #sim_copy$internals$levels_grid_big <- levels_grid_big

  # if there are extra runs to do
  if (nrow(not_run) > 0) {

    sim_copy$internals$levels_grid_big <- not_run

    # for new run, do only levels / sim_ids that have not been run

    sim_copy$results <- NULL
    sim_copy$errors <- NULL
    sim_copy$warnings <- NULL

    # run
    sim_copy <- run(sim_copy)

    # combine results/errors/warnings of original run and update
    if (!is.character(sim_copy$results)) {
      if (!is.character(sim$results)) {
        sim_copy$results <- rbind(sim$results, sim_copy$results)
      }
      sim_copy$results <- sim_copy$results[order(sim_copy$results$sim_uid),]
    }
    else {
      sim_copy$results <- sim$results
    }
    if (!is.character(sim_copy$errors)) {
      if (!is.character(sim$errors)) {
        sim_copy$errors <- rbind(sim$errors, sim_copy$errors)
      }
      sim_copy$errors <- sim_copy$errors[order(sim_copy$errors$sim_uid),]
    }
    else {
      sim_copy$errors <- sim$errors
    }
    if (!is.character(sim_copy$warnings)) {
      if (!is.character(sim$warnings)) {
        sim_copy$warnings <- rbind(sim$warnings, sim_copy$warnings)
      }
      sim_copy$warnings <- sim_copy$warnings[order(sim_copy$warnings$sim_uid),]
    }
    else {
      sim_copy$warnings <- sim$warnings
    }

  } else {
    warning("No additional simulations to run.")
  }

  sim_copy$internals$levels_grid_big <- levels_grid_big

  return (sim_copy)

}
