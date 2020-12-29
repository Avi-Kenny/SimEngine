#' Add a "method" function
#'
#' @description Add a "method" function to your simulation object.
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param name A name for the method function
#' @param fn A function that performs some analytic method. The function must
#'     accept an object returned by a creator function as its first argument
#' @return The original simulation object with the new method function added
#' @examples
#' # First, we create a simulation object and add creator function:
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#'
#' # Like add_creator(), there are two ways to use add_method(). The first is to
#' # declare a function and add it to simba later:
#'
#' estimator_1 <- function (dat) { mean(dat) }
#' sim %<>% add_method(estimator_1)
#'
#' # The second is to do both at the same time:
#'
#' sim %<>% add_method("estimator_2", function(dat) {
#'   var(dat)
#' })
#'
#' # With either option, you can test your function as follows:
#'
#' dat <- sim$creators$create_data(10)
#' sim$methods$estimator_1(dat)
#' sim$methods$estimator_2(dat)
#' @export
add_method <- function(sim_obj, ...) UseMethod("add_method")

#' @export
add_method.simba <- function(sim_obj, ...) {

  handle_errors(sim_obj, "is.simba")

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
