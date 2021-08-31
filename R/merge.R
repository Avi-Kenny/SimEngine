#' #' Merge simulation results
#' #'
#' #' @description !!!!! This function is unfinished and undocumented
#' #' @param sim_1 A simulation object of class \code{sim_obj}, usually created
#' #'     by \code{\link{new_sim}}
#' #' @param sim_2 A simulation object of class \code{sim_obj}, usually created
#' #'     by \code{\link{new_sim}}
#' #' @return A simulation object containing the results from both sim_1 and
#' #'     sim_2
#' #' @examples
#' #' TO DO
#' #' @noRd
#' merge.sim_obj <- function(sim_1, sim_2, ...) {
#'
#'   handle_errors(sim_1, "is.sim_obj")
#'   handle_errors(sim_2, "is.sim_obj")
#'
#'   sim <- sim_1
#'
#'   sim$results <- rbind(sim_1$results, sim_2$results)
#'
#'   if (class(sim$errors)!="data.frame" && class(sim_2$errors)=="data.frame") {
#'     sim$errors <- sim_2$errors
#'   }
#'
#'   if (class(sim$errors)=="data.frame" && class(sim_2$errors)=="data.frame") {
#'     # !!!!! Currently an issue where one error displays twice
#'     sim$errors <- rbind(sim$errors, sim_2$errors)
#'   }
#'
#'   return (sim)
#'
#' }
