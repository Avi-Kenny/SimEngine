#' Create a new simulation object
#'
#' @description Create a new simulation object. This is typically the first
#'     function to be called when running a simulation using \pkg{SimEngine}. Most
#'     other \pkg{SimEngine} functions take a simulation object as their first
#'     argument.
#' @return A simulation object, of class \code{sim_obj}
#' @seealso
#' Visit \url{https://avi-kenny.github.io/SimEngine/} for more information on how to
#'     use the \pkg{SimEngine} simulation framework.
#' @examples
#' sim <- new_sim()
#' sim
#' @export
new_sim <- function() {

  # First check if dependencies are installed
  for (pkg in c("magrittr", "dplyr", "parallel", "pbapply", "data.table",
                "rlang", "methods")) {
    if (!requireNamespace(pkg, quietly=TRUE)) {
      stop(paste0(
        "You need to install the package '", pkg, "' for SimEngine to work."
      ))
    }
  }
  rm(pkg)

  # Create "blank" simulation object
  ..seed <- as.integer(1e9*runif(1))
  ..e <- .GlobalEnv
  ..sim <- list(
    config = list(
      num_sim = 10,
      parallel = "none",
      n_cores = NA,
      packages = NULL,
      stop_at_error = FALSE,
      seed = ..seed,
      progress_bar = TRUE,
      batch_levels = NA
    ),
    levels = list("no levels"=TRUE),
    levels_grid = data.frame(level_id=1),
    results = "Simulation has not been run yet.",
    results_complex = NA,
    errors = "Simulation has not been run yet.",
    warnings = "Simulation has not been run yet.",
    internals = list(
      levels_types = FALSE,
      levels_shallow = list("no levels"=TRUE),
      levels_prev = list(),
      tid = NA,
      sim_var = "",
      update_sim = FALSE,
      env_calling = parent.frame()
    ),
    vars = list(
      seed = ..seed,
      env = new.env(),
      num_sim_total = 10,
      run_state = "pre run"
    ),
    script = NULL
  )

  # Create batch_cache and set initial values
  assign(x="..batch_cache", value=new.env(), envir=..sim$vars$env)
  assign(x="batch_levels", value=NA,
         envir=get(x="..batch_cache", envir=..sim$vars$env))

  # Create flags container and set initial values
  assign(x="..flag_batch_n_cores", value=F, envir=..sim$vars$env)
  assign(x="..flag_batch_update", value=F, envir=..sim$vars$env)

  # Create a (hidden) global reference to the simulation environment that can be
  #     searched for via get() by functions (currently use_method and batch)
  #     that need to access this environment but doesn't take sim as an argument
  assign(x="..env", value=..sim$vars$env, envir=..e)

  class(..sim) <- "sim_obj"
  rm(..e)
  rm(..seed)

  return (..sim)

}
