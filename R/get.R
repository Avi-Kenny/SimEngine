#' Get the value of a simulation object variable
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param variable The name of the internal variable
#' @examples
#' !!!!! TO DO
#' @export
run <- function(sim_obj, variable) UseMethod("get")

#' @export
get.simba <- function(sim_obj, variable) {

  switch(
    variable,
    "total_runtime" = { return(sim_obj$internals$total_runtime) },
    "start_time" = { return(sim_obj$internals$start_time) },
    "end_time" = { return(sim_obj$internals$end_time) }, # !!!!! add error handling
    {
      stop("Invalid variable name")
    }
  )

}
