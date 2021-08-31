#' Access internal simulation variables
#'
#' @description This is a "getter function" that returns the value of an
#'     internal simulation variable. Do not change any of these variables
#'     manually.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \link{new_sim}
#' @param var If this argument is omitted, \code{vars()} will return a list
#'     containing all available internal variables. If this argument is
#'     provided, it should equal one of the following character strings:
#'     \itemize{
#'
#'     \item{\code{seed}: the simulation seed; see \link{set_config} for more
#'     info on seeds.}
#'
#'     \item{\code{env}: a reference to the environment in which individual
#'     simulation replicates are run (advanced)}
#'
#'     \item{\code{num_sim_total}: The total number of simulation replicates
#'     for the simulation. This is particularly useful when a simulation is
#'     being run in parallel on a cluster computing system as a job array and
#'     the user needs to know the range of task IDs.}
#'
#'     \item{\code{run_state}: A character string describing the "run state" of
#'     the simulation. This will equal one of the following: "pre run" (the
#'     simulation has not yet been run), "run, no errors" (the simulation ran
#'     and had no errors), "run, some errors" (the simulation ran and had some
#'     errors), "run, all errors" (the simulation ran and all replicates had
#'     errors).}
#'
#'    }
#' @details \itemize{
#'   \item{You can also access simulation variables through sim$vars, where
#'     \code{sim} is your simulation object (see examples).}
#' }
#' @return The value of the internal variable.
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000)
#' )
#' sim %<>% set_config(num_sim=10)
#' vars(sim, "num_sim_total") %>% print()
#' sim$vars$num_sim_total %>% print()
#' vars(sim) %>% print()
#' @export
vars <- function(sim, var) UseMethod("vars")

#' @export
vars <- function(sim, var) {

  # Error handling
  handle_errors(sim, "is.sim_obj")
  if (!missing(var)) {
    valid_vars <- c("seed", "env", "num_sim_total", "run_state")
    handle_errors(var, "is.in", other=valid_vars)
  }

  # Parse list of variables
  v <- sim$vars
  if (!missing(var)) {
    v <- v[[var]]
  }

  return (v)

}
