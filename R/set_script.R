#' Add a "simulation script" function
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @return The original simulation object with the new script function added.
#'     The script should be a function that returns a list of key-value pairs. !!!!! continue
#' @examples
#' !!!!! TO DO
#' @export
set_script <- function(sim_obj, ...) UseMethod("set_script")

#' @export
set_script.simba <- function(sim_obj, ...) {

  if (length(list(...)) > 1){
    stop("`set_script` takes only a single argument")
  }

  fn <- list(...)[[1]]

  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  sim_obj$script <- fn

  return (sim_obj)

}
