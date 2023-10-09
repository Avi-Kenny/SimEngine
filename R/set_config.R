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
#' @param parallel Boolean; if set to TRUE, \pkg{SimEngine} will run one
#'     simulation per core. if set to FALSE, code will not be parallelized. See
#'     \url{https://avi-kenny.github.io/SimEngine/parallelization/} for an
#'     overview of how parallelization works in \pkg{SimEngine}. This option
#'     will be automatically set to TRUE if the simulation is being run on a
#'     cluster computing system.
#' @param n_cores An integer; determines the number of cores on which the
#'     simulation will run if using parallelization. Defaults to one fewer than
#'     the number of available cores.
#' @param packages A character vector of packages to load and attach
#' @param stop_at_error Boolean; if set to TRUE, the simulation will stop if it
#'     encounters an error in any single replicate Useful for debugging.
#' @param progress_bar Boolean; if set to FALSE, the progress bar that is
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
#'   \code{Sys.time} or dynamically retrieving external data may produce
#'   different results on different runs.}
#' }
#' @param batch_levels Either NULL or a character vector. If the
#'     \code{\link{batch}} function is being used within the simulation script,
#'     this should contain the names of the simulation levels that are used
#'     within the \code{\link{batch}} function code block. If no simulation
#'     levels are used within the \code{\link{batch}} function code block,
#'     specify NULL. See the documentation for the \code{\link{batch}} function.
#' @param return_batch_id Boolean. If set to TRUE, the batch_id will be included
#'     as part of the simulation results
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
  sim, num_sim=1000, parallel=FALSE, n_cores=NA, packages=NULL,
  stop_at_error=FALSE, progress_bar=TRUE, seed=as.integer(1e9*runif(1)),
  batch_levels=NA, return_batch_id=FALSE
) {
  UseMethod("set_config")
}

#' @export
set_config.sim_obj <- function(
  sim, num_sim=1000, parallel=FALSE, n_cores=NA, packages=NULL,
  stop_at_error=FALSE, progress_bar=TRUE, seed=as.integer(1e9*runif(1)),
  batch_levels=NA, return_batch_id=FALSE
) {

  if (length(as.list(match.call()))==2) {
    stop("No configuration options were specified")
  }

  if (!missing(num_sim)) {
    handle_errors(num_sim, "is.numeric")
    sim$config[["num_sim"]] <- num_sim
    sim$vars$num_sim_total <- num_sim_total(sim)
  }

  if (!missing(parallel)) {
    if (parallel=="outer") {
      warning(paste0("parallel='outer' is deprecated; please use parallel=TRUE",
                     " instead."))
      parallel <- TRUE
    } else if (parallel=="none") {
      warning(paste0("parallel='none' is deprecated; please use parallel=FALSE",
                     " instead."))
      parallel <- FALSE
    }
    handle_errors(parallel, "is.boolean")
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
    sim$vars$session_info = utils::sessionInfo()
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

  if (!missing(batch_levels)) {
    if(!is.null(batch_levels)) {
      handle_errors(batch_levels, "is.character.vec")
    }
    sim$config[["batch_levels"]] <- batch_levels
    assign(x="batch_levels", value=batch_levels,
           envir=get(x="..batch_cache", envir=sim$vars$env))
  }

  if (!missing(return_batch_id)) {
    handle_errors(return_batch_id, "is.boolean")
    sim$config[["return_batch_id"]] <- return_batch_id
  }

  if (!missing(num_sim) || !missing(n_cores) || !missing(batch_levels)) {
    sim$internals$level_batch_map <- update_level_batch_map(sim)
    sim$internals$sim_uid_grid <- update_sim_uid_grid(sim)
    sim$vars$num_sim_total <- num_sim_total(sim)
  }

  set.seed(sim$config[["seed"]])

  return (sim)

}
