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
#' @param datasets A string; either "one" or "many". If set to "one", the same
#'     dataset will be used for all simulations. If set to "many", a new
#'     dataset will be generated for each simulation.
#' @param parallel String; either "outer", "inner", or "none". Controls which
#'     sections of the code are parallelized. Setting to "outer" will run one
#'     simulation per core. Setting to "inner" will allow for parallelization
#'     within a single simulation replicate. Setting to "none" will not
#'     parallelize any code. See
#'     \url{https://avi-kenny.github.io/simba/parallelization} for an overview
#'     of how parallelization works in \pkg{simba}.
#' @param packages A character vector of packages to load and attach
#' @param stop_at_error A Boolean. If set to TRUE, the simulation will
#'     stop if it encounters an error in any single replicate Useful for
#'     debugging.
#' @param seed An integer; seeds allow for reproducible simulation results.
#'     Normally, when a given simulation is run multiple times, it will give
#'     the same results each time unless the seed is changed using
#'     \code{set_config}.
#' @param dir A directory (given as a character string) where simulation files
#'     should be stored; if this option is not set, files will be stored in the
#'     current working directory.
#' @details \itemize{
#'   \item{If a user specifies, for example, \code{set_config(seed=4)}, this
#'       seed is used twice by \pkg{simba}. First, \pkg{simba} executes
#'       \code{set.seed(4)} at the end of the \code{set_config} call. Second,
#'       when \link{run} is called, \pkg{simba} will execute
#'       \code{set.seed(as.integer(4*sim_uid))} at the start of simulation
#'       replicate, where \code{sim_uid} is the unique identifier corresponding
#'       to that replicate. This is necessary to ensure that results are
#'       reproducible even when simulations involve parallelization.}
#'   \item{Even if seeds are used, not all code will be reproducible. For
#'       example, a simulation that involves getting the current date/time with
#'       \code{Sys.time()} may produce different results on different runs.}
#'   \item{Setting seeds is not currently with inner parallelization
#'       (\code{set_config(parallel="inner")}).}
#' }
#' @return The original simulation object with a modified configuration
#' @examples
#' sim <- new_sim()
#' sim %<>% set_config(
#'   num_sim = 10
#' )
#' sim
#' @export
set_config <- function(
  sim_obj, num_sim=1000, datasets="many", parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, seed=1, dir=getwd()
) UseMethod("set_config")

#' @export
set_config.simba <- function(
  sim_obj, num_sim=1000, datasets="many", parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, seed=1, dir=getwd()
) {

  handle_errors(sim_obj, "is.simba")

  if (length(as.list(match.call()))==2) {
    stop("No configuration options were specified")
  }

  if (!missing(num_sim)) {
    handle_errors(num_sim, "is.numeric")
    sim_obj$config[["num_sim"]] = num_sim
    sim_obj$internals$num_sim_total <- nrow(sim_obj$levels_grid)*num_sim
  }

  if (!missing(datasets)) {
    handle_errors(datasets, "is.in", other=c("one","many"))
    sim_obj$config[["datasets"]] = datasets
  }

  if (!missing(parallel)) {
    handle_errors(parallel, "is.in", other=c("outer","inner","none"))
    sim_obj$config[["parallel"]] = parallel
  }

  if (!missing(n_cores)) {
    handle_errors(n_cores, "is.numeric")
    sim_obj$config[["n_cores"]] = n_cores
  }

  if (!missing(packages)) {
    handle_errors(packages, "is.character.vec")
    sim_obj$config[["packages"]] = packages
    for (pkg in packages) {
      do.call("library", list(pkg))
    }
  }

  if (!missing(stop_at_error)) {
    handle_errors(stop_at_error, "is.boolean")
    sim_obj$config[["stop_at_error"]] = stop_at_error
  }

  if (!missing(seed)) {
    handle_errors(seed, "is.numeric")
    sim_obj$config[["seed"]] = seed
  }

  if (!missing(dir)) {
    handle_errors(dir, "is.character")
    sim_obj$config[["dir"]] = dir
  }

  set.seed(as.integer(seed))

  return (sim_obj)

}
