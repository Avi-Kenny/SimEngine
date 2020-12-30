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
#' @param dir A directory (given as a character string) where simulation files
#'     should be stored; if this option is not set, files will be stored in the
#'     current working directory.
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
  parallel_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, dir=getwd()
) UseMethod("set_config")

#' @export
set_config.simba <- function(
  sim_obj, num_sim=1000, datasets="many", parallel="none",
  parallel_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, dir=getwd()
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
    handle_errors(datasets, "is.in", c("one","many"))
    sim_obj$config[["datasets"]] = datasets
  }

  if (!missing(parallel)) {
    handle_errors(parallel, "is.in", c("outer","inner","none"))
    sim_obj$config[["parallel"]] = parallel
  }

  if (!missing(parallel_cores)) {
    handle_errors(parallel_cores, "is.numeric")
    sim_obj$config[["parallel_cores"]] = parallel_cores
  }

  if (!missing(packages)) {
    handle_errors(packages, "is.character.vector")
    sim_obj$config[["packages"]] = packages
    for (pkg in packages) {
      do.call("library", list(pkg))
    }
  }

  if (!missing(stop_at_error)) {
    handle_errors(stop_at_error, "is.boolean")
    sim_obj$config[["stop_at_error"]] = stop_at_error
  }

  if (!missing(dir)) {
    handle_errors(dir, "is.character")
    sim_obj$config[["dir"]] = dir
  }

  return (sim_obj)

}
