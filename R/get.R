#' Get the value of a simulation object internal variable
#'
#' @description Get the value of a simulation object internal variable
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param variable The name of the internal variable. Options include the
#'     following:\n
#'     - hey \n
#'     - there
#' @examples
#' sim <- new_sim()
#' sim %>% get("num_sim_total")
#' sim %<>% set_levels(alpha=c(2,3,4))
#' sim %>% get("num_sim_total")
#' @export
get <- function(sim_obj, variable) UseMethod("get")

#' @export
get.simba <- function(sim_obj, variable) {

  handle_errors(sim_obj, "is.simba")

  switch(
    variable,
    "num_sim_total" = { return(sim_obj$internals$num_sim_total) },
    "total_runtime" = { return(sim_obj$internals$total_runtime) },
    "start_time" = { return(sim_obj$internals$start_time) },
    "end_time" = { return(sim_obj$internals$end_time) },
    {
      stop("Invalid variable name")
    }
  )

}
