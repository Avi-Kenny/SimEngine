#' Set the "simulation script"
#'
#' @description Specify a function to be used as the "simulation script". The
#'     simulation script is a function that runs a single simulation replicate
#'     and returns the results.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param fn A function that runs a single simulation replicate and returns the
#'     result. The result must be a list of key-value pairs. The values
#'     themselves can either be simple (numeric, character, etc.) or complex
#'     (matrices, lists, etc.). The function body can contain references to the
#'     special objects `L` (simulation levels) and `C` (simulation constants).
#'     See examples.
#' @return The original simulation object with the new "simulation script"
#'     function added.
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the set_script() function.
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#' sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
#' sim %<>% add_method("estimator_2", function(dat) { var(dat) })
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "estimator" = c("estimator_1", "estimator_2")
#' )
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- do.call(L$estimator, list(dat))
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim$results
#' @export
set_script <- function(sim_obj, ...) UseMethod("set_script")

#' @export
set_script.simba <- function(sim_obj, ...) {

  handle_errors(sim_obj, "is.simba")

  if (substr(sim_obj$internals$run_state, 1, 3) == "run") {
    stop(paste("A simulation object's script cannot be changed after the",
               "simulation has been run."))
  }

  if (length(list(...)) > 1) {
    stop(paste("`set_script` takes only two arguments, a simulation script and",
               "a function"))
  }

  fn <- list(...)[[1]]

  if (!is.function(fn)) {
    stop("`fn` must be a function")
  }

  environment(fn) <- sim_obj$internals$env
  sim_obj$script <- fn
  assign(x="..script", value=fn, envir=sim_obj$internals$env)

  return (sim_obj)

}
