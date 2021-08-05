#' Modify the simulation configuration
#'
#' @description This function sets configuration options for the simulation. If
#'     the 'packages' argument is specified, all packages will be loaded and
#'     attached via \code{library} when \code{set_config} is called. Multiple
#'     calls to \code{set_config} will only overwrite configuration options that
#'     are specified in the subsequent calls, leaving others in place. You can
#'     see the current configuration via \code{print(sim)}, where \code{sim} is
#'     your simulation object.
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param num_sim An integer; the number of simulations to conduct for each
#'     level combination
#' @param parallel String; one of c("outer", "inner", "none"). Controls which
#'     sections of the code are parallelized. Setting to "outer" will run one
#'     simulation per core. Setting to "inner" will allow for parallelization
#'     within a single simulation replicate. Setting to "none" will not
#'     parallelize any code. See
#'     \url{https://avi-kenny.github.io/simba/parallelization} for an overview
#'     of how parallelization works in \pkg{simba}. This option will be ignored
#'     if the simulation is being run on a cluster computing system.
#' @param packages A character vector of packages to load and attach
#' @param stop_at_error A Boolean. If set to TRUE, the simulation will
#'     stop if it encounters an error in any single replicate Useful for
#'     debugging.
#' @param seed An integer; seeds allow for reproducible simulation results. If a
#'     seed is specified, then consecutive runs of the same simulation with the
#'     same seed will lead to identical results (under normal circumstances). If
#'     a seed was not set in advance by the user, \pkg{simba} will set a random
#'     seed, which can later be retrieved using the \link{vars} function. See
#'     details for further info.
#' @param n_cores An integer; determines the number of CPUs on which the simulation
#'     will run if using parallelization. Defaults to one fewer than the number of
#'     available CPUs on the current host.
#' @param dir A directory (given as a character string) where simulation files
#'     should be stored; if this option is not set, files will be stored in the
#'     current working directory.
#' @details \itemize{
#'   \item{If a user specifies, for example, \code{set_config(seed=4)}, this
#'   seed is used twice by \pkg{simba}. First, \pkg{simba} executes
#'   \code{set.seed(4)} at the end of the \code{set_config} call. Second, this
#'   seed is used to generate a new set of seeds, one for each simulation
#'   replicate. Each of these seeds is set in turn (or in parallel) when
#'   \link{run} is called.}
#'   \item{Even if seeds are used, not all code will be reproducible. For
#'   example, a simulation that involves getting the current date/time with
#'   \code{Sys.time()} or dynamically retrieving external data may produce
#'   different results on different runs.}
#' }
#' @return The original simulation object with a modified configuration
#' @examples
#' sim <- new_sim()
#' sim %<>% set_config(
#'   num_sim = 10,
#'   seed = 2112
#' )
#' sim
#' @export
set_config <- function(
  sim_obj, num_sim=1000, parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, seed=NA, dir=getwd()
) UseMethod("set_config")

#' @export
set_config.simba <- function(
  sim_obj, num_sim=1000, parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, seed=NA, dir=getwd()
) {

  handle_errors(sim_obj, "is.simba")

  if (length(as.list(match.call()))==2) {
    stop("No configuration options were specified")
  }

  if (!missing(num_sim)) {
    handle_errors(num_sim, "is.numeric")
    sim_obj$config[["num_sim"]] <- num_sim
    sim_obj$vars$num_sim_total <- nrow(sim_obj$levels_grid)*num_sim
  }

  if (!missing(parallel)) {
    handle_errors(parallel, "is.in", other=c("outer","inner","none"))
    sim_obj$config[["parallel"]] <- parallel
  }

  if (!missing(n_cores)) {
    handle_errors(n_cores, "is.numeric")
    sim_obj$config[["n_cores"]] <- n_cores
  }

  if (!missing(packages)) {
    handle_errors(packages, "is.character.vec")
    sim_obj$config[["packages"]] <- packages
    for (pkg in packages) {
      do.call("library", list(pkg))
    }
  }

  if (!missing(stop_at_error)) {
    handle_errors(stop_at_error, "is.boolean")
    sim_obj$config[["stop_at_error"]] <- stop_at_error
  }

  if (!missing(seed)) {
    handle_errors(seed, "is.numeric")
    sim_obj$config[["seed"]] <- seed
  }

  if (!missing(dir)) {
    handle_errors(dir, "is.character")
    sim_obj$config[["dir"]] <- dir
  }

  set.seed(sim_obj$config[["seed"]])

  return (sim_obj)

}
