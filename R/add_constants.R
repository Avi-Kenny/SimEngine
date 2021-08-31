#' Add one or more simulation constants
#'
#' @param sim A simulation object of class \code{sim_obj}, usually created by
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
add_constants <- function(sim, ...) UseMethod("add_constants")

#' @export
add_constants.sim_obj <- function(sim, ...) {

  handle_errors(sim, "is.sim_obj")

  sim$constants <- c(sim$constants, list(...))

  return (sim)

}
