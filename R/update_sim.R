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

  # if (!keep_errors) { sim$errors <- "No errors" }

  if (Sys.getenv("sim_run")!="") {

    # If on cluster, delete old results/errors/warnings
    sim$results <- NULL
    sim$results_complex <- list()
    sim$errors <- NULL
    sim$warnings <- NULL

  } else {

    # Remove inactive results/errors/warnings
    sim <- delete_inactive_rwe(sim)

  }

  # Set update flags
  sim$internals$update_sim <- TRUE
  assign(x="..flag_batch_update", value=T, envir=sim$vars$env)

  if (sum(sim$internals$sim_uid_grid$to_run)>0) {

    # Create and run a copy of the sim
    sim_copy <- sim
    sim_copy$results <- NULL
    sim_copy$results_complex <- list()
    sim_copy$errors <- NULL
    sim_copy$warnings <- NULL
    sim_copy %<>% run()

    # Combine results/errors/warnings of original run and updated run
    # This also sets character states (e.g. sim$errors<-"No errors"), if needed
    sim <- combine_original_with_update(
      sim = sim,
      results_new = sim_copy$results,
      results_complex_new = sim_copy$results_complex,
      errors_new = sim_copy$errors,
      warnings_new = sim_copy$warnings
    )

    # Reset sim_uid_grid$to_run
    sim$internals$sim_uid_grid$to_run <- F

    # Update run_state variable
    sim$vars$run_state <- update_run_state(sim)

  }

  return (sim)

}
