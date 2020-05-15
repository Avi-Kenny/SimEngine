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

  # Load creators/methods
  for (obj in c("creators", "methods")) {
    if (length(sim_obj[[obj]])!=0) {
      for (i in 1:length(sim_obj[[obj]])) {
        assign(
          x = names(sim_obj[[obj]])[i],
          value = (sim_obj[[obj]])[[i]],
          envir = globalenv() # !!!!! Temp fix
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

    # Export creators/methods to cluster
    for (obj in c("creators", "methods")) {
      if (length(sim_obj[[obj]])!=0) {
        for (i in 1:length(sim_obj[[obj]])) {
          cluster_export <- c(cluster_export, names(sim_obj[[obj]])[i])
        }
      }
    }
    envir <- environment()
    parallel::clusterExport(cl, cluster_export, envir)
    clusterCall(cl, function(x) {.libPaths(x)}, .libPaths())
    parallel::clusterEvalQ(cl, sapply(packages, function(p) {
      do.call("library", list(p))
    }))
  }

  run_script <- function(i) {

    # Set up references to levels_grid row and constants
    L <- levels_grid[i,]
    C <- sim_obj$constants

    # !!!!! This is janky AF. Use environments properly
    eval(parse(text=c("use_method <-", deparse(use_method)))) # !!!!! why is this needed?
    eval(parse(text=c("s_copy <-", deparse(sim_obj$scripts[[script]]))))

    start_time <- Sys.time()
    script_results <- tryCatch(
      expr = {
        do.call(
          s_copy,
          args = list(as.list(L), as.list(C)) # !!!!! This may throw a warning if script does not take any arguments
        )
      },
      error = function(e) { return(e) }
    )
    runtime <- as.numeric(difftime(Sys.time(), start_time), units="secs")

    return (list(
      "sim_uid" = i,
      "runtime" = runtime,
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

  # Separate errors from results
  results_lists_ok <- list()
  results_lists_err <- list()
  for (i in 1:length(results_lists)) {
    if (is(results_lists[[i]]$results, "error")) {
      results_lists_err[[length(results_lists_err)+1]] <- results_lists[[i]]
    } else {
      results_lists_ok[[length(results_lists_ok)+1]] <- results_lists[[i]]
    }
  }

  # Convert summary statistics to data frame
  # !!!!! Make this an option; some lists can't be copmressed into a DF
  if (length(results_lists_ok)>0) {
    results_df <- data.frame(
      matrix(
        unlist(results_lists_ok),
        nrow = length(results_lists_ok),
        byrow = TRUE
      )
    )
    names(results_df) <- c("sim_uid", "runtime",
                           names(results_lists_ok[[1]]$results))
  }
  if (length(results_lists_err)>0) {
    errors_df <- data.frame(
      matrix(
        unlist(results_lists_err),
        nrow = length(results_lists_err),
        byrow = TRUE
      )
    )
    names(errors_df) <- c("sim_uid", "runtime", "message", "call")
  }
  # !!!!! errors_df is not a valid data frame; parse df manually?

  # Join results data frames with `levels_grid`and attach to sim_obj
  if (exists("results_df")) {
    results_df <- dplyr::inner_join(levels_grid, results_df, by="sim_uid")
    sim_obj$results <- results_df
  } else {
    sim_obj$results <- NULL # !!!!! Need to distinguish "sim has not been run" from "sim was run and resulted in 100% errors"
  }

  # Attach errors to sim_obj
  if (exists("errors_df")) {
    sim_obj$errors <- errors_df
  } else {
    sim_obj$errors <- "No errors"
  }

  return (sim_obj)

}
