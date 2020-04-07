#' Set simulation levels
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param ... One or more key-value pairs representing simulation levels (see
#'     below)
#' @return The original simulation object with the old set of levels replaced
#'     with the new set
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(
#'   "rho" = c(0.1, 0.2),
#'   "eta" = c(2,3)
#' )
#' !!!!! continue example
#' @export
set_levels <- function(sim_obj, ...) UseMethod("set_levels")

#' @export
set_levels.simba <- function(sim_obj, ...) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  # !!!!! Add error handling for "..."

  if (length(list(...))==0) { stop("No levels supplied") }
  sim_obj$levels <- list(...)

  return (sim_obj)

}
