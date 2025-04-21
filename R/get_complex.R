#' Access internal simulation variables
#'
#' @description Extract complex simulation data from a simulation object
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param sim_uid The unique identifier of a single simulation replicate. This
#'     corresponds to the \code{sim_uid} column in \code{sim$results}.
#' @return The value of the complex simulation result data corresponding to the
#'     supplied \code{sim_uid}
#' @examples
#' sim <- new_sim()
#' sim %<>% set_levels(n=c(10, 100, 1000))
#' create_data <- function(n) {
#'   x <- runif(n)
#'   y <- 3 + 2*x + rnorm(n)
#'   return(data.frame("x"=x, "y"=y))
#' }
#' sim %<>% set_config(num_sim=2)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   model <- lm(y~x, data=dat)
#'   return(list(
#'     "beta0_hat" = model$coefficients[[1]],
#'     "beta1_hat" = model$coefficients[[2]],
#'     ".complex" = list(
#'       "model" = model,
#'       "cov_mtx" = vcov(model)
#'     )
#'   ))
#' })
#' sim %<>% run()
#' c5 <- get_complex(sim, sim_uid=5)
#' print(summary(c5$model))
#' print(c5$cov_mtx)
#' @export

#' @export
get_complex <- function(sim, sim_uid) {
  UseMethod("get_complex")
}

#' @export
get_complex.sim_obj <- function(sim, sim_uid) {

  # Error handling
  if (sim$vars$run_state == "pre run") {
    stop("Simulation has not been run yet.")
  }
  if (sim$vars$run_state == "run, all errors") {
    stop("100% of simulations had errors.")
  }
  if (length(sim$results_complex)==0) {
    stop("This simulation does not have any complex return data.")
  }
  handle_errors(sim_uid, "is.numeric")

  res <- sim$results_complex[[paste0("sim_uid_",sim_uid)]]

  if (is.null(res)) {
    stop(paste0("There is no result corresponding to sim_uid=", sim_uid, "."))
  } else {
    return (res)
  }

}
