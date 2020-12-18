#' Add a "method" function
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
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
add_method <- function(sim_obj, ...) UseMethod("add_method")

#' @export
add_method.simba <- function(sim_obj, ...) {

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

  environment(fn) <- sim_obj$internals$env
  sim_obj$methods[[name]] <- fn
  assign(x=name, value=fn, envir=sim_obj$internals$env)

  return (sim_obj)

}
