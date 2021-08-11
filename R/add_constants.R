#' Add one or more simulation constants
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param ... Key-value pairs will be added as "simulation constants" (i.e.
#'     objects that don't change across simulations). Keys should be strings.
#'     The purpose of this (rather than "hard-coding" constants in your scripts)
#'     is to serve as an organizational container to easily change constants
#'     later, and so that constants are automatically available on each cluster
#'     node if you decide to run your simulation code in parallel
#' @return The original simulation object with added constants
#' @examples
#' sim <- new_sim()
#' sim %<>% add_constants(alpha=4, beta=c(1,2,3))
#' @export
add_constants <- function(sim_obj, ...) UseMethod("add_constants")

#' @export
add_constants.simba <- function(sim_obj, ...) {

  handle_errors(sim_obj, "is.simba")

  sim_obj$constants <- c(sim_obj$constants, list(...))

  return (sim_obj)

}
