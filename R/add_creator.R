#' Add a "creator" function
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param name A name for the dataset-creating function
#' @param fn A function that creates a simulated dataset
#' @return The original simulation object with the new creator function added
#' @examples
#' sim <- new_sim()
#' sim %<>% add_creator(
#'   "create_rct",
#'   function(n, sigma) {
#'     x <- runif(n)
#'     y <- 4*x + rnorm(n, mean=0, sd=sigma)
#'     return (data.frame(x=x,y=y))
#'   }
#' )
#' !!!!! continue example
#' @export
add_creator <- function(sim_obj, name, fn) UseMethod("add_creator")

#' @export
add_creator.simba <- function(sim_obj, name, fn) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }
  if (!(is.character(name) & length(name)==1)) {
    stop("`name` must be a character string")
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  sim_obj$creators[[name]] <- fn

  return (sim_obj)

}
