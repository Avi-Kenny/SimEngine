#' Run the simulation
#'
#' @param sim_obj A simulation object of class "simba", usually created by
#'     new_sim()
#' @param script The name of a simulation script, added using add_script()
#' @param sim_uids A vector of sim_uids that represent simulations to run. If
#'     omitted, all simulations are run. # update this !!!!!
#' @examples
#' !!!!! TO DO
#' @export
run <- function(sim_obj, script, ...) UseMethod("run")

#' @export
run.simba <- function(sim_obj, script, ...) {

  sim_obj$internals$start_time <- Sys.time()

  # Get reference to current environment
  env <- environment()

  o_args <- list(...)
  if (!is.null(o_args$sim_uids)) {
    # !!!!! add error handling
    sim_uids <- o_args$sim_uids
  } else if (!is.na(sim_obj$internals$tid)) {
    sim_uids <- sim_obj$internals$tid
  } else {
    sim_uids <- 1:sim_obj$internals$num_sim_total
  }

  # Create levels_grid_big
  levels_grid_big <- expand.grid(list(
    "level_id" = sim_obj$levels_grid$level_id,
    "sim_id" = 1:sim_obj$config$num_sim
  ))

  levels_grid_big <- dplyr::inner_join(
    levels_grid_big,
    sim_obj$levels_grid,
    by = "level_id"
  )
  levels_grid_big <- dplyr::arrange(levels_grid_big, level_id, sim_id)
  names_2 <- names(levels_grid_big)
  levels_grid_big <- cbind(1:nrow(levels_grid_big), levels_grid_big)
  names(levels_grid_big) <- c("sim_uid", names_2)
  sim_obj$internals$levels_grid_big <- levels_grid_big

  # Load creators/methods
  for (obj in c("creators", "methods")) {
    if (length(sim_obj[[obj]])!=0) {
      for (i in 1:length(sim_obj[[obj]])) {
        assign(
          x = names(sim_obj[[obj]])[i],
          value = (sim_obj[[obj]])[[i]],
          envir = env
        )
      }
    }
  }

  # Set up parallelization code
  if (sim_obj$config$parallel %in% c("inner", "outer")) {
    # !!!!! Should this apply only to "inner" parallelization ?????

    packages <- sim_obj$config$packages
    n_available_cores <- parallel::detectCores()
    if (sim_obj$config$parallel_cores==0) {
      # !!!!! If detectCores() runs on a different machine than the code runs on, this will be problematic
      n_cores <- n_available_cores - 1
    } else {
      if (sim_obj$config$parallel_cores>n_available_cores) {
        n_cores <- n_available_cores
        warning(paste(sim_obj$config$parallel_cores, "cores requested but only",
                      n_available_cores, "cores available"))
      } else {
        n_cores <- sim_obj$config$parallel_cores
      }
    }

    cl <- parallel::makeCluster(n_cores)
    cluster_export <- c("sim_obj", "packages")

    # Export creators/methods to cluster
    for (obj in c("creators", "methods")) {
      if (length(sim_obj[[obj]])!=0) {
        for (i in 1:length(sim_obj[[obj]])) {
          cluster_export <- c(cluster_export, names(sim_obj[[obj]])[i])
        }
      }
    }
    parallel::clusterExport(cl, cluster_export, env)
    parallel::clusterCall(cl, function(x) {.libPaths(x)}, .libPaths())
    parallel::clusterEvalQ(cl, sapply(packages, function(p) {
      do.call("library", list(p))
    }))
  }

  run_script <- function(i) {

    ..start_time <- Sys.time()

    # Set up references to levels_grid_big row and constants
    C <- sim_obj$constants
    L <- as.list(sim_obj$internals$levels_grid_big[i,])
    levs <- names(sim_obj$levels)
    for (j in 1:length(levs)) {
      if (sim_obj$internals$levels_types[j]) {
        L[[levs[j]]] <- sim_obj$levels[[levs[j]]][[L[[levs[j]]]]]
      }
    }
    rm(levs)

    # Declare script copy dynamically
    # !!!!! Currently errors are being logged with call "..script_copy()"
    eval(parse(text=c("..script_copy <-", deparse(sim_obj$scripts[[script]]))))

    if (sim_obj$config$stop_at_error==TRUE) {
      ..script_copy()
    } else {
      script_results <- tryCatch(
        expr = { ..script_copy() },
        error = function(e) { return(e) }
      )
    }

    runtime <- as.numeric(difftime(Sys.time(), ..start_time), units="secs")

    return (list(
      "sim_uid" = i,
      "runtime" = runtime,
      "results" = script_results
    ))

  }

  # Run simulations
  if (sim_obj$config$parallel=="outer") {
    # Run in parallel
    results_lists <- parLapply(cl, sim_uids, run_script)
  } else {
    # Run serially
    results_lists <- lapply(sim_uids, run_script)
  }

  # Stop cluster
  if (exists("cl")) { parallel::stopCluster(cl) }

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

  # Generate completion message
  num_ok <- length(results_lists_ok)
  num_err <- length(results_lists_err)
  pct_err <- round((100*num_err)/(num_err+num_ok),0)
  if (pct_err==0) {
    comp_msg <- "Done. No errors detected.\n"
  } else {
    comp_msg <- paste0(
      "Done. Errors detected in ", pct_err, "% of simulation replicates.\n"
    )
  }

  sim_obj$internals$end_time <- Sys.time()
  sim_obj$internals$total_runtime <- as.numeric(
    difftime(sim_obj$internals$end_time, sim_obj$internals$start_time),
    units = "secs"
  )

  # Convert summary statistics to data frame
  # !!!!! In addition to sim$results and sim$errors, create a place to store
  #       other non-flat simulation data
  if (num_ok>0) {

    first <- results_lists_ok[[1]]
    one_list <- c(list(
      "sim_uid" = first$sim_uid,
      "runtime" = first$runtime
    ), first$results)
    results_df <- as.data.frame(one_list, stringsAsFactors=FALSE)
    results_df <- results_df[0,]

    # !!!!! Speed this up ?????
    for (i in 1:length(results_lists_ok)) {
      r <- results_lists_ok[[i]]
      results_df[nrow(results_df)+1,] <- c(r$sim_uid, r$runtime, r$results)
    }

  }

  # Convert errors to data frame
  if (num_err>0) {

    errors_df <- data.frame(
      "sim_uid" = integer(),
      "runtime" = double(),
      "message" = character(),
      "call" = character(),
      stringsAsFactors=FALSE
    )

    for (i in 1:length(results_lists_err)) {

      if (is.null(results_lists_err[[i]]$results$call)) {
        call <- NA
      } else {
        call <- deparse(results_lists_err[[i]]$results$call)
      }

      errors_df[nrow(errors_df)+1,] <- list(
        "sim_uid" = results_lists_err[[i]]$sim_uid,
        "runtime" = results_lists_err[[i]]$runtime,
        "message" = results_lists_err[[i]]$results$message,
        "call" = call
      )
    }

  }

  # Join results data frames with `levels_grid_big`and attach to sim_obj
  if (exists("results_df")) {
    results_df <- dplyr::inner_join(
      sim_obj$internals$levels_grid_big,
      results_df,
      by = "sim_uid"
    )
    sim_obj$results <- results_df
  }

  # Join results data frames with `levels_grid_big`and attach to sim_obj
  if (exists("errors_df")) {
    errors_df <- dplyr::inner_join(
      sim_obj$internals$levels_grid_big,
      errors_df,
      by = "sim_uid"
    )
    sim_obj$errors <- errors_df
  }

  # Set states
  if (exists("results_df") && exists("errors_df")) {
    sim_obj$internals$run_state <- "run, some errors"
  } else if (exists("results_df")) {
    sim_obj$internals$run_state <- "run, no errors"
    sim_obj$errors <- "No errors."
  } else if (exists("errors_df")) {
    sim_obj$internals$run_state <- "run, all errors"
    sim_obj$results <- "Errors detected in 100% of simulation replicates."
  } else {
    stop("An unknown error occurred.")
  }

  cat(comp_msg)

  return (sim_obj)

}
