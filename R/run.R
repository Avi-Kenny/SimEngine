#' Run the simulation
#'
#' @param sim_obj A simulation object created by new_sim()
#' @param script The name of a simulation script, added using add_script()
#' @examples
#' !!!!! TO DO
#' @export
run <- function(sim_obj, script) UseMethod("run")

#' @export
run.simba <- function(sim_obj, script) {

  # Set up levels_grid
  levels_grid_1 <- expand.grid(sim_obj$levels, stringsAsFactors=FALSE)
  levels_names_1 <- names(levels_grid_1)
  levels_grid_1 <- cbind(1:nrow(levels_grid_1), levels_grid_1)
  names(levels_grid_1) <- c("level_id", levels_names_1)
  levels_grid <- expand.grid(list(
    "level_id" = levels_grid_1$level_id,
    "sim_id" = 1:sim_obj$config$num_sim
  ))
  levels_grid <- dplyr::inner_join(levels_grid, levels_grid_1, by="level_id")
  levels_names <- names(levels_grid)
  levels_grid <- dplyr::arrange(levels_grid, level_id, sim_id)
  levels_grid <- cbind(1:nrow(levels_grid), levels_grid)
  names(levels_grid) <- c("sim_uid",levels_names)

  # Load creators and methods
  for (obj in c("creators", "methods")) {
    if (length(sim_obj[[obj]])!=0) {
      for (i in 1:length(sim_obj[[obj]])) {
        assign(
          x = names(sim_obj[[obj]])[i],
          value = (sim_obj[[obj]])[[i]]
        )
      }
    }
  }

  # Set up parallelization code
  if (!sim_obj$config$parallel=="none") {

    packages <- sim_obj$config$packages
    n_cores <- parallel::detectCores() - 1 # !!!!! Make this an argument
    cl <- parallel::makeCluster(n_cores)
    cluster_export <- c("sim_obj", "levels_grid", "use_method", "packages")
    for (obj in c("creators", "methods")) {
      for (i in 1:length(sim_obj[[obj]])) {
        cluster_export <- c(cluster_export, names(sim_obj[[obj]])[i])
      }
    }
    envir <- environment()
    parallel::clusterExport(cl, cluster_export, envir)
    parallel::clusterEvalQ(cl, sapply(packages, function(p) {
      do.call("library", list(p))
    }))
  }

  run_script <- function(i) {

    # Set up references to levels_grid row and constants
    L <- levels_grid[i,]
    C <- sim_obj$constants

    # !!!!! This is janky AF. Use environments properly
    eval(parse(text=c("s_copy <-", deparse(sim_obj$scripts[[script]]))))
    eval(parse(text=c("use_method <-", deparse(use_method))))

    script_results <- do.call(
      what = s_copy,
      args = list(as.list(L), as.list(C)) # !!!!! This may throw a warning if script does not take any arguments
    )

    return (list(
      "sim_uid" = i,
      "results" = script_results
    ))

  }

  # Run simulations
  if (sim_obj$config$parallel=="outer") {
    # run parallel code
    results_lists <- parLapply(cl, 1:nrow(levels_grid), run_script)
  } else {
    results_lists <- lapply(1:nrow(levels_grid), run_script)
  }

  # Stop cluster
  if (exists("cl")) { stopCluster(cl) }

  # Convert summary statistics to data frame
  results_df <- data.frame(
    matrix(
      unlist(results_lists),
      nrow = length(results_lists),
      byrow = TRUE
    )
  )
  names(results_df) <- c("sim_uid", names(results_lists[[1]]$results))

  # # Join `results` with `levels_grid`
  results_df <- dplyr::inner_join(levels_grid, results_df, by="sim_uid")

  sim_obj$results <- results_df

  # Return results
  return (sim_obj)

}
