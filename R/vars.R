#' Access internal simulation variables
#'
#' @description This is a "getter function" that returns the value of an
#'     internal simulation variable.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param var If this argument is omitted, \code{vars()} will return a list
#'     containing all available internal variables. If this argument is
#'     provided, it should equal one of the following character strings:
#'     \itemize{
#'
#'     \item{\code{config}: a list representing the current simulation
#'     configuration. This will typically be set using \code{set_config()}.}
#'
#'     \item{\code{env}: a reference to the environment in which individual
#'     simulation replicates are run (advanced)}
#'
#'     \item{\code{num_sim_total}: The total number of simulation replicates
#'     for the simulation}
#'
#'     \item{\code{run_state}: A character string describing the "run state" of
#'     the simulation. This will equal one of the following: "pre run" (the
#'     simulation has not yet been run), "run, no errors" (the simulation ran
#'     and had no errors), "run, some errors" (the simulation ran and had some
#'     errors), "run, all errors" (the simulation ran and all replicates had
#'     errors).}
#'
#'    }
#' @return The value of the internal variable.
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000)
#' )
#' sim %<>% set_config(num_sim=10)
#' value <- vars(sim, "num_sim_total")
#' value
#' all_values <- vars(sim)
#' all_values
#' @export
vars <- function(sim_obj, var) {

  # Error handling
  handle_errors(sim_obj, "is.simba")
  if (!missing(var)) {
    valid_vars <- c("config", "env", "num_sim_total", "run_state")
    handle_errors(var, "is.in", other=valid_vars)
  }

  # Parse list of variables
  v <- list(
    config = sim_obj$config,
    env = sim_obj$internals$env,
    num_sim_total = sim_obj$internals$num_sim_total,
    run_state = sim_obj$internals$run_state
  )
  if (!missing(var)) {
    v <- v[[var]]
  }

  return (v)

}
