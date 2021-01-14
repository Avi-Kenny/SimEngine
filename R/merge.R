#' Merge simulation results
#'
#' @description !!!!! TO DO
#' @param sim_obj_1 A simulation object of class \code{simba}, usually created
#'     by \link{new_sim}
#' @param sim_obj_2 A simulation object of class \code{simba}, usually created
#'     by \link{new_sim}
#' @return A simulation object containing the results from both sim_obj_1 and
#'     sim_obj_2
#' @examples
#' TO DO
#' @export
merge.simba <- function(sim_obj_1, sim_obj_2, ...) {

  handle_errors(sim_obj, "is.simba")

  sim_obj <- sim_obj_1

  sim_obj$results <- rbind(sim_obj_1$results, sim_obj_2$results)

  if (class(sim_obj$errors)!="data.frame" && class(sim_obj_2$errors)=="data.frame") {
    sim_obj$errors <- sim_obj_2$errors
  }

  if (class(sim_obj$errors)=="data.frame" && class(sim_obj_2$errors)=="data.frame") {
    # !!!!! Currently an issue where one error displays twice
    sim_obj$errors <- rbind(sim_obj$errors, sim_obj_2$errors)
  }

  return (sim_obj)

}
