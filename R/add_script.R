#' Add a "simulation script" function
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @return The original simulation object with the new script function added.
#'     The script should be a function that returns a list of key-value pairs. !!!!! continue
#' @examples
#' !!!!! TO DO
#' @export
add_script <- function(sim_obj, ...) UseMethod("add_script")

#' @export
add_script.simba <- function(sim_obj, ...) {

  if (length(list(...))==1) {
    name <- deparse(substitute(...))
    fn <- list(...)[[1]]
  }
  if (length(list(...))==2) {
    name <- list(...)[[1]]
    fn <- list(...)[[2]]
  }

  if (!(is.character(name) && length(name)==1)) {
    stop("`name` must be a character string")
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  sim_obj$scripts[[name]] <- fn

  return (sim_obj)

}
