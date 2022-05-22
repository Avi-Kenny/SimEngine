#' Modify the simulation configuration
#'
#' @description This function sets configuration options for the simulation. If
#'     the 'packages' argument is specified, all packages will be loaded and
#'     attached via \code{library} when \code{set_config} is called. Multiple
#'     calls to \code{set_config} will only overwrite configuration options that
#'     are specified in the subsequent calls, leaving others in place. You can
#'     see the current configuration via \code{print(sim)}, where \code{sim} is
#'     your simulation object.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param num_sim An integer; the number of simulations to conduct for each
#'     level combination
#' @param parallel A string; one of c("outer", "inner", "none"). Controls which
#'     sections of the code are parallelized. Setting to "outer" will run one
#'     simulation per core. Setting to "inner" will allow for parallelization
#'     within a single simulation replicate. Setting to "none" will not
#'     parallelize any code. See
#'     \url{https://avi-kenny.github.io/SimEngine/parallelization/} for an
#'     overview of how parallelization works in \pkg{SimEngine}. This option
#'     will be ignored (and automatically set to "cluster") if the simulation is
#'     being run on a cluster computing system.
#' @param n_cores An integer; determines the number of CPUs on which the simulation
#'     will run if using parallelization. Defaults to one fewer than the number of
#'     available CPUs on the current host.
#' @param packages A character vector of packages to load and attach
#' @param stop_at_error A Boolean. If set to TRUE, the simulation will
#'     stop if it encounters an error in any single replicate Useful for
#'     debugging.
#' @param progress_bar A Boolean. If set to FALSE, the progress bar that is
#'     normally displayed while the simulation is running is suppressed.
#' @param seed An integer; seeds allow for reproducible simulation results. If a
#'     seed is specified, then consecutive runs of the same simulation with the
#'     same seed will lead to identical results (under normal circumstances). If
#'     a seed was not set in advance by the user, \pkg{SimEngine} will set a
#'     random seed, which can later be retrieved using the \code{\link{vars}}
#'     function. See details for further info.
#' @details \itemize{
#'   \item{If a user specifies, for example, \code{set_config(seed=4)}, this
#'   seed is used twice by \pkg{SimEngine}. First, \pkg{SimEngine} executes
#'   \code{set.seed(4)} at the end of the \code{set_config} call. Second, this
#'   seed is used to generate a new set of seeds, one for each simulation
#'   replicate. Each of these seeds is set in turn (or in parallel) when
#'   \code{\link{run}} is called.}
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
  sim, num_sim=1000, parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, progress_bar=TRUE, seed=NA
) UseMethod("set_config")

#' @export
set_config.sim_obj <- function(
  sim, num_sim=1000, parallel="none",
  n_cores=parallel::detectCores()-1, packages=NULL,
  stop_at_error=FALSE, progress_bar=TRUE, seed=NA
) {

  handle_errors(sim, "is.sim_obj")

  if (length(as.list(match.call()))==2) {
    stop("No configuration options were specified")
  }

  if (!missing(num_sim)) {
    handle_errors(num_sim, "is.numeric")
    sim$config[["num_sim"]] <- num_sim
    sim$vars$num_sim_total <- nrow(sim$levels_grid)*num_sim
  }

  if (!missing(parallel)) {
    handle_errors(parallel, "is.in", other=c("outer","inner","none"))
    sim$config[["parallel"]] <- parallel
  }

  if (!missing(n_cores)) {
    handle_errors(n_cores, "is.numeric")
    sim$config[["n_cores"]] <- n_cores
  }

  if (!missing(packages)) {
    handle_errors(packages, "is.character.vec")
    sim$config[["packages"]] <- packages
    for (pkg in packages) { do.call("library", list(pkg)) }
  }

  if (!missing(stop_at_error)) {
    handle_errors(stop_at_error, "is.boolean")
    sim$config[["stop_at_error"]] <- stop_at_error
  }

  if (!missing(progress_bar)) {
    handle_errors(progress_bar, "is.boolean")
    sim$config[["progress_bar"]] <- progress_bar
  }

  if (!missing(seed)) {
    handle_errors(seed, "is.numeric")
    sim$config[["seed"]] <- seed
    sim$vars[["seed"]] <- seed
  }

  set.seed(sim$config[["seed"]])

  return (sim)

}
