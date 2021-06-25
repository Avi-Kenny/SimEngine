#' Create a new simulation object
#'
#' @description Create a new simulation object. This is typically the first
#'     function to be called when running a simulation using \pkg{simba}. Most
#'     other \pkg{simba} functions take a simulation object as their first
#'     argument.
#' @return A simulation object, of class \code{simba}
#' @seealso
#' Visit \url{https://avi-kenny.github.io/simba} for more information on how to
#'     use the \pkg{simba} simulation framework.
#' @examples
#' sim <- new_sim()
#' sim
#' @export
new_sim <- function() {

  # First check if dependencies are installed
  for (pkg in c("magrittr", "parallel", "pbapply", "data.table")) {
    if (!requireNamespace(pkg, quietly=TRUE)) {
      stop(paste0(
        "You need to install the package '", pkg, "' for simba to work."
      ))
    }
  }
  rm(pkg)

  # Create "blank" simulation object
  ...sim_obj <- list(
    config = list(
      num_sim = 10,
      datasets = "many",
      parallel = "none",
      n_cores = parallel::detectCores() - 1,
      packages = NULL,
      stop_at_error = FALSE,
      seed = as.integer(10^6*runif(1)),
      dir = getwd()
    ),
    constants = list(),
    levels = list("no levels"=TRUE),
    levels_grid = data.frame(level_id=1),
    results = "Simulation has not been run yet.",
    results_complex = NA,
    errors = "Simulation has not been run yet.",
    warnings = "Simulation has not been run yet.",

    # run_state can be: "pre run", "run, no errors", "run, some errors",
    #     "run, all errors"
    internals = list(
      env = environment(), # new.env()
      levels_types = FALSE,
      levels_shallow = list("no levels"=TRUE),
      levels_prev = list(),
      num_sim_prev = NA,
      num_sim_cumulative = 0,
      tid = NA,
      num_sim_total = 10,
      sim_var = "",
      run_state = "pre run",
      update_sim = FALSE
    ),
    creators = list(),
    methods = list(),
    script = NULL,
    results = NULL,
    errors = NULL
  )

  class(...sim_obj) <- "simba"

  return (...sim_obj)

}
