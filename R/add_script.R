#' Add a "simulation script" function
#'
#' @return The original simulation object with the new script function added
#' @examples
#' !!!!! TO DO
#' @export
add_script <- function(sim_obj, ...) UseMethod("add_script")

#' @export
add_script.simba <- function(sim_obj, ...) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  if (length(list(...))==1) {
    name <- deparse(substitute(...))
    fn <- list(...)[[1]]
  }
  if (length(list(...))==2) {
    name <- list(...)[[1]]
    fn <- list(...)[[2]]
  }

  if (!(is.character(name) & length(name)==1)) {
    stop("`name` must be a character string")
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  # !!!!! parse()
  sim_obj$scripts[[name]] <- fn

  return (sim_obj)

}
