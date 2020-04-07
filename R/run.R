#' Run the simulation
#'
#' @param sim_obj A simulation object created by new_sim()
#' @examples
#' sim <- new_sim()
#' sim %>% run()
#' !!!!! continue example
#' @export
run <- function(sim_obj) UseMethod("run")

#' @export
run.simba <- function(sim_obj) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  # !!!!! temporary
  print("These are my results")

}
