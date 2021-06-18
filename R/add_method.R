#' Add a "method" function
#'
#' @description Add a "method" function to your simulation object. A method
#'     function is just a function, and can be used anywhere that you would
#'     normally write and use a regular global function. The advantages of
#'     explicitly adding a method function to your simulation (rather than
#'     declaring and using a function within your simulation script) are that
#'     (1) you can use the method function as a simulation level, and (2)
#'     parallelization is automated. Often, the method function will be a
#'     statistical method that you want to test (e.g. an estimator), and will
#'     take in a dataset returned by a creator function as its first argument;
#'     however, this is not always the case.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param name A name for the method function
#' @param fn A method function
#' @details \itemize{
#'   \item{As with \link{add_creator}, there are two ways to use
#'     \code{add_method}. If two arguments are supplied (\code{sim_obj} and
#'     \code{fn}), you can create a function separately and add it to your
#'     simulation object later. If three arguments are supplied, you can do both
#'     at the same time, using an anonymous function for the \code{fn} argument.
#'     See examples.}
#'   \item{Your method will be stored in \code{sim_obj$methods}. If you added a
#'     method called \code{estimator_1}, you can test it out by running
#'     \code{sim$creators$estimator_1()}. See examples.}
#' }
#' @return The original simulation object with the new method function added
#' @examples
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#'
#' # The first way to use add_method is to declare a function and add it to
#' # your simulation object later:
#'
#' estimator_1 <- function (dat) { mean(dat) }
#' sim %<>% add_method(estimator_1)
#'
#' # The second way is to do both at the same time:
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
add_method <- function(sim_obj, name, fn) UseMethod("add_method")

#' @export
add_method.simba <- function(sim_obj, name, fn) {

  .e <- sim_obj # Throws error if argument doesn't exist
  handle_errors(sim_obj, "is.simba")
  if (missing(name) && missing(fn)) {
    stop("You must provide a function to add_method.")
  }

  # Handle case when only one of {name,fn} is given
  if (missing(name)) {
    name <- deparse(substitute(fn))
  }
  if (missing(fn)) {
    fn <- name
    name <- deparse(substitute(name))
  }

  handle_errors(name, "is.character")
  handle_errors(fn, "is.function")

  environment(fn) <- sim_obj$internals$env
  sim_obj$methods[[name]] <- fn
  assign(x=name, value=fn, envir=sim_obj$internals$env)

  return (sim_obj)

}
