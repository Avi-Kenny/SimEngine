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
#'   num_sim = 10,
#'   parallel = "none"
#' )
#' @export
set_config <- function(sim_obj, ...) UseMethod("set_config")

#' @export
set_config.simba <- function(
  sim_obj, num_sim=NA, datasets=NA, parallel=NA, packages=NA, stop_at_error=NA
) {

  # !!!!! Add error handing for other arguments

  # !!!!! Rewrite this more efficiently with `...`

  if (!is.na(num_sim)) { sim_obj$config$num_sim = num_sim }
  if (!is.na(stop_at_error)) { sim_obj$config$stop_at_error = stop_at_error }
  if (!is.na(datasets)) { sim_obj$config$datasets = datasets }
  if (!is.na(parallel)) { sim_obj$config$parallel = parallel }
  if (!is.na(packages[1])) { sim_obj$config$packages = packages }

  return (sim_obj)

}
