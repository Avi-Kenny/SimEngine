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
#' @param dir Directory (given as a character string) where simulation files
#'     should be stored; defaults to current working directory
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

  valid_opts <- c("num_sim", "datasets", "parallel", "parallel_cores",
                  "packages", "progress", "stop_at_error", "dir")

  # !!!!! Make a note in the docs that stop_at_error will not work on cluster

  for (i in 1:length(o_args)) {
    name <- names(o_args[i])
    value <- o_args[[i]]
    if (!(name %in% valid_opts)) {
      stop(paste0("'",name,"' is not a valid configuration option."))
      # !!!!! Add additional error handling for each config option
    } else {
      sim_obj$config[[name]] = value
    }
  }

  if (!is.null(o_args$num_sim)) {
    sim_obj$internals$num_sim_total <- nrow(sim_obj$levels_grid)*o_args$num_sim
  }

  return (sim_obj)

}
