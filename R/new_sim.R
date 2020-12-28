#' Initialize a new simulation object
#'
#' @description !!!!! TO DO
#' @param config A list. Contains simulation configuration information
#' @return A simulation object of class "simba"
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
      num_sim = 1000,
      datasets = "many",
      parallel = "none",
      parallel_cores = 0,
      packages = c(),
      progress = "none",
      stop_at_error = FALSE,
      dir = getwd()
    ),
    constants = list(),
    levels = list("no levels"=TRUE),
    levels_grid = data.frame(level_id=1),
    results = "Simulation has not been run yet.",
    errors = "Simulation has not been run yet.",
    warnings = "Simulation has not been run yet.",

    # run_state can be: "pre run", "run, no errors", "run, some errors",
    #     "run, all errors"
    internals = list(
      env = environment(), # new.env()
      levels_types = FALSE,
      levels_shallow = list("no levels"=TRUE),
      #levels_shallow = list(),
      levels_prev = list(),
      num_sim_prev = NA,
      tid = NA,
      num_sim_total = 1,
      sim_var = "",
      run_state = "pre run",
      update = FALSE
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
