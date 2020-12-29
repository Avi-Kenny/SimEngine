#' Get the value of a simulation object internal variable
#'
#' @description Get the value of a simulation object internal variable
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param variable The name of the internal variable. Options include:
#'     \code{num_sim_total} (the total number of simulation replicates to be
#'     run, equal to the configuration variable num_sim times the number of
#'     simulation levels), \code{start_time} (the time at which the simulation
#'     began), and \code{end_time} (the time at which the simulation finished),
#'     \code{total_runtime} (the difference between start_time and end_time, in
#'     seconds).
#' @return The value of the internal variable. The data type depends on the
#'     \code{variable} argument.
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
  handle_errors(variable, "is.in", c("num_sim_total", "total_runtime", "start_time", "end_time"))

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
