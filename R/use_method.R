#' Use a method
#'
#' @description This function calls the specified method, passing along any
#'     arguments that have been specified in \code{args}. It will typically be
#'     used in conjunction with the special object L to dynamically run methods
#'     that have been included as simulation levels. This function is a wrapper
#'     around do.call and is used in a similar manner. See examples.
#' @param method A character string naming a function that has been added to
#'     your simulation object via \code{\link{add_method}}
#' @param args A list of arguments to be passed onto \code{method}
#' @return The result of the call to \code{method}
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the use_method function.
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#' sim %<>% add_method("est_mean_1", function(dat) { mean(dat) })
#' sim %<>% add_method("est_mean_2", function(dat) { var(dat) })
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "estimator" = c("est_mean_1", "est_mean_2")
#' )
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- use_method(L$estimator, list(dat))
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim$results
#' @export
use_method <- function(method, args=list()) {

  # Not using handle_errors so that function call is returned
  if (!(is.character(method) && length(method)==1)) {
    stop("`method must be a character string`")
  }

  # Locate the method and its environment
  ..env <- get("..env", envir=.GlobalEnv)
  ..m <- get(method, envir=..env, inherits=FALSE)

  # Call to do.call
  return (do.call(..m, args, envir=..env))

}
