
#' Initialize a new simulation object
#'
#' @param config A list. Contains simulation configuration information
#' @return A simulation object
#' @examples
#' sim <- new_sim()
new_sim <- function(config=NA) {

  sim_obj <- list(

    "config" = list(
      "num_sim" = 5,
      "datasets" = "many",
      "parallel" = "inner"
    ),

    "levels" = list(
      "dimension_1" = c(33,44,55),
      "dimension_2" = c(10,100,1000)
    ),

    "constants" = list(
      "n" = 10,
      "constant_2" = 123
    )

  )

  return (sim_obj)

}



#' Modify simulation configuration
#'
#' @param sim_obj A simulation object returned by new_sim()
#' @param num_sim Number of simulations to conduct for each level combination
#' @param levels A list of vectors defining "simulation levels". If num_sim=5,
#'     then five simulations will be conducted for each combination of levels.
#'     For example, if levels=list("psi"=c(1,2), "eta"=c(8,9)), then five
#'     simulations will be conducted for (psi=1,eta=8), five for (psi=2,eta=8),
#'     five for (psi=1,eta=9), and five for (psi=2,eta=9).
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
#'   levels = list("rho"=c(0.1, 0.2), "eta"=c(2,3))
#' )
config <- function(
  sim_obj, num_sim=NA, levels=NA, datasets=NA, parallel=NA, constants=NA
) {

  if (!is.na(num_sim)) { sim_obj$config$num_sim = num_sim }
  if (!is.na(levels)) { sim_obj$config$levels = levels }
  if (!is.na(datasets)) { sim_obj$config$datasets = datasets }
  if (!is.na(parallel)) { sim_obj$config$parallel = parallel }
  if (!is.na(constants)) { sim_obj$config$constants = constants }

  return (sim_obj)

}
