#' Add a "creator" function
#'
#' @description Add a "creator" function to your simulation object.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param name A name for the dataset-creating function
#' @param fn A function that creates a simulated dataset
#' @details
#' There are two ways to use add_creator(). The first is to declare a function
#'     and add it to your simulation object later. The second is to do both at
#'     the same time. See examples.
#' @return The original simulation object with the new creator function added
#' @examples
#' # There are two ways to use add_creator(). The first is to declare a function
#' # and add it to simba later:
#'
#' sim <- new_sim()
#' create_data <- function (n) { rpois(n, lambda=5) }
#' sim %<>% add_creator(create_data)
#'
#' # The second is to do both at the same time:
#'
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) {
#'   rpois(n, lambda=5)
#' })
#'
#' # With either option, you can test your function as follows:
#'
#' sim$creators$create_data(10)
#' @export
add_creator <- function(sim_obj, name, fn) UseMethod("add_creator")

#' @export
add_creator.simba <- function(sim_obj, name, fn) {

  handle_errors(sim_obj, "is.simba")


  if (!missing(name) && !missing(fn))


  if (length(list(...))==1) {
    name <- deparse(substitute(...))
    fn <- list(...)[[1]]
  }
  if (length(list(...))==2) {
    name <- list(...)[[1]]
    fn <- list(...)[[2]]
  }

  handle_errors(name, "is.string")
  handle_errors(fn, "is.function")

  environment(fn) <- sim_obj$internals$env
  sim_obj$creators[[name]] <- fn # !!!!! Is this redundant ?????
  assign(x=name, value=fn, envir=sim_obj$internals$env)

  return (sim_obj)

}
