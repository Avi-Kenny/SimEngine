#' Set the "simulation script"
#'
#' @description Specify a function to be used as the "simulation script". The
#'     simulation script is a function that runs a single simulation replicate
#'     and returns the results.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param fn A function that runs a single simulation replicate and returns the
#'     results. The results must be a list of key-value pairs. Values are
#'     categorized as simple (a number, a character string, etc.) or complex
#'     (vectors, dataframes, lists, etc.). Complex data must go inside a key
#'     called ".complex" and the associated value must be a list (see examples).
#'     The function body can contain references to the special objects \code{L}
#'     (simulation levels) and \code{C} (simulation constants) (see examples).
#'     The keys must be valid R names (see ?make.names).
#' @return The original simulation object with the new "simulation script"
#'     function added.
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the set_script function.
#' sim <- new_sim()
#' create_data <- function(n) { rpois(n, lambda=5) }
#' est_mean <- function(dat, type) {
#'   if (type=="M") { return(mean(dat)) }
#'   if (type=="V") { return(var(dat)) }
#' }
#' sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- est_mean(dat=dat, type=L$est)
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim$results
#'
#' # If you need to return complex result data (vectors, dataframes, lists,
#' # etc.), use the construct ".complex"=list().
#' sim <- new_sim()
#' sim %<>% set_levels(n=c(4,9))
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- rnorm(L$n)
#'   mtx <- matrix(dat, nrow=sqrt(length(dat)))
#'   return (list(
#'     "mean" = mean(dat),
#'     "det" = det(mtx),
#'     ".complex" = list(dat=dat, mtx=mtx)
#'   ))
#' })
#' sim %<>% run()
#'
#' @export
set_script <- function(sim, fn) UseMethod("set_script")

#' @export
set_script.sim_obj <- function(sim, fn) {

  handle_errors(sim, "is.sim_obj")
  handle_errors(fn, "is.function")

  if (substr(sim$vars$run_state, 1, 3) == "run") {
    stop(paste("A simulation object's script cannot be changed after the",
               "simulation has been run."))
  }

  # Add "global" objects to simulation object run environment (excluding
  #     simulation object); these are not necessarily in the global environment,
  #     but are in the environment the new_sim() call is executed within.
  for (obj_name in ls(sim$internals$env_calling)) {
    obj <- get(x=obj_name, envir=sim$internals$env_calling)
    if (!(class(obj)=="sim_obj")) {
      assign(x=obj_name, value=obj, envir=sim$vars$env)
    }
  }

  environment(fn) <- sim$vars$env
  sim$script <- fn
  assign(x="..script", value=fn, envir=sim$vars$env)

  return (sim)

}
