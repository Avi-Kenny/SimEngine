#' Modify the simulation configuration
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param num_sim Number of simulations to conduct for each level combination
#' @param datasets String; either "one" or "many". If set to "one", the same
#'     dataset will be used for all simulations. If set to "many", a new
#'     dataset will be generated for each simulation
#' @param parallel String; Either "inner", "outer", or "none".Controls which
#'     sections of the code are parallelized (i.e. for speed gains on
#'     multicore/cluster computing systems). Setting to "outer" will run one
#'     simulation per core. Setting to "inner" will parallelize operations
#'     within a single simulation. Setting to "none" will not parallelize any
#'     code (useful for debugging).
#' @param packages A character vector of packages to load on the cluster
#' @param stop_at_error Boolean (FALSE by default); if TRUE, simulation will
#'     stop after it encounters an error in any single run. Useful for
#'     debugging.
#' @return The original simulation object with a modified configuration
#' @examples
#' sim <- new_sim()
#' sim %<>% set_config(
#'   num_sim = 10
#' )
#' @export
set_config <- function(sim_obj, ...) UseMethod("set_config")

#' @export
set_config.simba <- function(sim_obj, ...) {

  o_args <- list(...)

  if (length(o_args)==0) { stop("No configuration options specified") }

  for (i in 1:length(o_args)) {
    name <- names(o_args[i])
    value <- o_args[[i]]
    if (!(name %in% c("num_sim", "stop_at_error", "datasets", "parallel",
                      "parallel_cores", "packages"))) {
      stop(paste0("'",name,"' is not a valid configuration option."))
      # !!!!! Add additional error handling for each config option
    } else {
      sim_obj$config[[name]] = value
    }
  }

  return (sim_obj)

}
