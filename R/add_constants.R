#' Add simulation constants
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param ... One or more key-value pairs representing simulation constants
#'     (any object that does not change across simulations).
#' @return The original simulation object with the new constants added
#' @examples
#' sim <- new_sim()
#' sim %<>% add_constants(
#'   "k" = 44,
#'   "tau" = c(0.25,0.5,0.75)
#' )
#' !!!!! continue example
#' @export
add_constants <- function(sim_obj, ...) UseMethod("add_constants")

#' @export
add_constants.simba <- function(sim_obj, ...) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  # !!!!! Add error handling for "..."

  if (length(list(...))==0) { stop("No constants supplied") }
  sim_obj$constants <- list(...)

  return (sim_obj)

}
