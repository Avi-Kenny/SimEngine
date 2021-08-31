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
#' sim %<>% add_creator("create_data", function(n) {
#'   x <- runif(n)
#'   y <- 3 + 2*x + rnorm(n)
#'   return(data.frame("x"=x, "y"=y))
#' })
#' sim %<>% set_levels("n"=c(10, 100, 1000))
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   model <- lm(y~x, data=dat)
#'   return (list(
#'     "beta1_hat" = model$coefficients[[2]],
#'     ".complex" = model
#'   ))
#' })
#' sim %<>% run()
#' sim$results %>% print()
#' get_complex(sim, 1) %>% print()
#' @export

#' @export
get_complex <- function(sim, sim_uid) UseMethod("get_complex")

#' @export
get_complex <- function(sim, sim_uid) {

  # Error handling
  handle_errors(sim, "is.sim_obj")
  if (sim$vars$run_state == "pre run")
    stop("Simulation has not been run yet.")
  if (sim$vars$run_state == "run, all errors")
    stop("100% of simulations had errors.")
  handle_errors(sim_uid, "is.numeric")
  # handle_errors(sim_uid, "is.in", other=sim$results$sim_uid, # !!!!! add error handling for sim_uid
  #               msg="sim_uid not found in results")

  if (length(sim_uid==1)) {
    return (sim$results_complex[[paste0("sim_uid_",sim_uid)]])
  } else {
    # !!!!! Options for sim_uid=NA or sim_uid=c(1,2) ?????
  }

}
