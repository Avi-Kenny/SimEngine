#' Merge simulation results
#'
#' @param sim_obj_1 A simulation object of class "simba", usually created by
#'     new_sim()
#' @param sim_obj_2 A simulation object of class "simba", usually created by
#'     new_sim()
#' @return A simulation object containing the results from both sim_obj_1 and
#'     sim_obj_1
#' @examples
#' TO DO
#' @export
merge <- function(sim_obj_1, sim_obj_2, ...) UseMethod("merge")

#' @export
merge.simba <- function(sim_obj_1, sim_obj_2, ...) {

  # !!!!! Error handling to make sure levels, constants, etc. are the same
  # !!!!! Also separately handle cases where sim_uids overlap vs not

  sim_obj <- sim_obj_1

  sim_obj$results <- rbind(sim_obj_1$results, sim_obj_2$results) # !!!!! This will only work for data frames

  if (class(sim_obj$errors)!="data.frame" && class(sim_obj_2$errors)=="data.frame") {
    sim_obj$errors <- sim_obj_2$errors
  }

  if (class(sim_obj$errors)=="data.frame" && class(sim_obj_2$errors)=="data.frame") {
    # !!!!! Currently an issue where one error displays twice
    sim_obj$errors <- rbind(sim_obj$errors, sim_obj_2$errors) # !!!!! This will only work for data frames
  }

  return (sim_obj)

}
