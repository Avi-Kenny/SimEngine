#' Modify simulation configuration
#'
#' @param sim_obj A simulation object returned by new_sim()
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
#' @return A simulation object with modified configuration
#' @examples
#' sim <- new_sim()
#' sim %<>% config(
#'   num_sim = 10,
#'   parallel = "none"
#' )
#' @export
config <- function(sim_obj, ...) UseMethod("config")

#' @export
config.simba <- function(
  sim_obj, num_sim=NA, datasets=NA, parallel=NA, constants=NA
) {

  if (!is.na(num_sim)) { sim_obj$config$num_sim = num_sim }
  if (!is.na(datasets)) { sim_obj$config$datasets = datasets }
  if (!is.na(parallel)) { sim_obj$config$parallel = parallel }
  if (!is.na(constants)) { sim_obj$config$constants = constants }

  return (sim_obj)

}
