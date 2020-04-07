#' Modify the simulation configuration
#'
#' @param sim_obj A simulation object created by new_sim()
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
#' @param constants A list containing any objects (numbers, vectors, data
#'     frames) that remain constant across all simulations.
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
  sim_obj, num_sim=NA, datasets=NA, parallel=NA, constants=NA
) {

  if (class(sim_obj)!="simba") {
    stop("`sim_obj` must be an object of class 'simba', returned by new_sim()")
  }

  # !!!!! Add error handing for other arguments

  if (!is.na(num_sim)) { sim_obj$config$num_sim = num_sim }
  if (!is.na(datasets)) { sim_obj$config$datasets = datasets }
  if (!is.na(parallel)) { sim_obj$config$parallel = parallel }
  if (!is.na(constants)) { sim_obj$config$constants = constants }

  return (sim_obj)

}
