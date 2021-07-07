#' Access internal simulation variables
#'
#' @description Extract complex simulation data from a simulation object
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param sim_uid The unique identifier of a single simulation replicate. This
#'     corresponds to the \code{sim_uid} column in \code{sim$results}.
#' @return The value of the complex simulation result data corresponding to the
#'     supplied \code{sim_uid}
#' @examples
#' sim <- new_sim()
#' !!!!! TO DO
#' @export

#' @export
get_complex <- function(sim_obj, sim_uid) UseMethod("get_complex")

#' @export
get_complex <- function(sim_obj, sim_uid) {

  # Error handling
  handle_errors(sim_obj, "is.simba")
  if (sim_obj$vars$run_state == "pre run")
    stop("Simulation has not been run yet.")
  if (sim_obj$vars$run_state == "run, all errors")
    stop("100% of simulations had errors.")
  handle_errors(sim_uid, "is.numeric")
  # handle_errors(sim_uid, "is.in", other=sim_obj$results$sim_uid, # !!!!! add error handling for sim_uid
  #               msg="sim_uid not found in results")

  # !!!!! Options for sim_uid=NA or sim_uid=c(1,2) ?????

  if (length(sim_uid==1)) {
    return (sim_obj$results_complex[[paste0("sim_uid_",sim_uid)]])
  } else {

  }

}
