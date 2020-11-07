#' Add one or more simulation constants
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param ... Key-value pairs will be added as "simulation constants" (i.e.
#'     objects that don't change across simulations). Keys should be strings.
#'     The purpose of this (rather than "hard-coding" constants in your scripts)
#'     is that constants are automateically available on each cluster node if
#'     you decide to run your simulation code in parallel
#' @return The original simulation object with added constants
#' @examples
#' # !!!!! TO DO
#' @export
add_constant <- function(sim_obj, ...) UseMethod("add_constant")

#' @export
add_constant.simba <- function(
  sim_obj, ...
) {

  sim_obj$constants <- c(sim_obj$constants, list(...))

  return (sim_obj)

}
