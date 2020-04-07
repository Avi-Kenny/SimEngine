#' Add a "method" function
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param name A name for the method function
#' @param fn A function that performs some analytic method. The function must
#'     accept an object returned by a creator function as its first argument
#' @return The original simulation object with the new method function added
#' @examples
#' sim <- new_sim()
#' sim %<>% add_method(
#'   "OLS",
#'   function(data) {
#'     model <- lm(y~x, data=data)
#'     return (model$coefficients[["x"]])
#'   }
#' )
#' !!!!! continue example
#' @export
add_method <- function(sim_obj, name, fn) UseMethod("add_method")

#' @export
add_method.simba <- function(sim_obj, name, fn) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }
  if (!(is.character(name) & length(name)==1)) {
    stop("`name` must be a character string")
  }
  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  sim_obj$methods[[name]] <- fn

  return (sim_obj)

}
