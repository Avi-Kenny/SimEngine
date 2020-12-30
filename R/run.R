#' Run the simulation
#'
#' @description !!!!! TO DO
#' @param sim_obj A simulation object of class \code{simba}, usually created by
#'     \link{new_sim}
#' @param sim_uids A vector of sim_uids that represent simulations to run. If
#'     omitted, all simulations are run. # update this !!!!!
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the run function.
#' sim <- new_sim()
#' sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
#' sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
#' sim %<>% add_method("estimator_2", function(dat) { var(dat) })
#' sim %<>% set_levels(
#'   "n" = c(10, 100, 1000),
#'   "estimator" = c("estimator_1", "estimator_2")
#' )
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- do.call(L$estimator, list(dat))
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim$results
#' @export
run <- function(sim_obj, ...) UseMethod("run")

#' @export
run.simba <- function(sim_obj, ...) {

  handle_errors(sim_obj, "is.simba")

  sim_obj$internals$start_time <- Sys.time()

  # All objects will be stored in this environment
  env <- sim_obj$internals$env

  o_args <- list(...)

  if (!sim_obj$internals$update){
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
  } else{
    sim_uids <- sim_obj$internals$levels_grid_big$sim_uid
  }


  # Set up parallelization code
  if (sim_obj$config$parallel %in% c("inner", "outer")) {

    ..packages <- c(sim_obj$config$packages, "magrittr")
    n_available_cores <- parallel::detectCores()
    if (sim_obj$config$parallel_cores==0) {
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

    # Create cluster and export everything in env
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, ls(env), env)
    parallel::clusterExport(cl, c("sim_obj","..packages"), environment())
    parallel::clusterCall(cl, function(x) {.libPaths(x)}, .libPaths())
    parallel::clusterEvalQ(cl, sapply(..packages, function(p) {
      do.call("library", list(p))
    }))
  }

  run_script <- function(i) {

    ..start_time <- Sys.time()

    # Set up references to levels row (L) and constants (C)
    assign(x="C", value=sim_obj$constants, envir=env)
    L <- as.list(sim_obj$internals$levels_grid_big[sim_obj$internals$levels_grid_big$sim_uid == i,])
    levs <- names(sim_obj$levels)
    for (j in 1:length(levs)) {
      if (sim_obj$internals$levels_types[j]) {
        L[[levs[j]]] <- sim_obj$levels[[levs[j]]][[L[[levs[j]]]]]
      }
    }
    assign(x="L", value=L, envir=env)
    rm(levs)
    rm(L)

    # Actually run the run
    # Use withCallingHandlers to catch all warnings and tryCatch to catch errors
    withCallingHandlers(
      {.gotWarnings <- character(0) # holds the warnings
      if (sim_obj$config$stop_at_error==TRUE & Sys.getenv("run")=="") {
        script_results <- do.call(what="..script", args=list(), envir=env)
      } else {
        script_results <- tryCatch(
          expr = do.call(what="..script", args=list(), envir=env),
          error = function(e){ return(e) }
        )
      }},
      warning = function(w){
        .gotWarnings <<- c(.gotWarnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    runtime <- as.numeric(difftime(Sys.time(), ..start_time), units="secs")

    return (list(
      "sim_uid" = i,
      "runtime" = runtime,
      "results" = script_results,
      "warnings" = .gotWarnings
    ))

  }

  # Set up progress bar
  pbapply::pboptions(type="txt", char="#", txt.width=40, style=3)

  # Run simulations
  if (sim_obj$config$parallel=="outer") {
    # Run in parallel
    results_lists <- pbapply::pblapply(sim_uids, run_script, cl=cl)
  } else {
    # Run serially
    results_lists <- pbapply::pblapply(sim_uids, run_script)
  }

  # Stop cluster
  if (exists("cl")) { parallel::stopCluster(cl) }

  # Separate errors from results
  # !!!!! Check to see how fast this is; this can probably be sped up
  results_lists_ok <- list()
  results_lists_warn <- list()
  results_lists_err <- list()
  for (i in 1:length(results_lists)) {
    if (is(results_lists[[i]]$results, "error")) {
      results_lists_err[[length(results_lists_err)+1]] <- results_lists[[i]]
    } else {
      results_lists_ok[[length(results_lists_ok)+1]] <- results_lists[[i]]
    }
    if (length(results_lists[[i]]$warnings) > 0){
      results_lists_warn[[length(results_lists_warn)+1]] <- results_lists[[i]]
    }
  }

  # Generate completion message
  num_ok <- length(results_lists_ok)
  num_err <- length(results_lists_err)
  pct_err <- round((100*num_err)/(num_err+num_ok),0)
  num_warn <- length(results_lists_warn)
  pct_warn <- round((100*num_warn)/(num_err + num_ok),0)
  if (pct_err==0 & pct_warn == 0) {
    comp_msg <- "Done. No errors or warnings detected.\n"
  } else if (pct_err > 0) {
    comp_msg <- paste0(
      "Done. Errors detected in ",
      pct_err,
      "% of simulation replicates. Warnings detected in ",
      pct_warn,
      "% of simulation replicates.\n"
    )
  } else {
    comp_msg <- paste0(
      "Done. No errors detected. Warnings detected in ",
      pct_warn,
      "% of simulation replicates.\n"
    )
  }

  # Convert summary statistics to data frame
  # !!!!! In addition to sim$results and sim$errors, create a place to store
  #       other non-flat simulation data
  if (num_ok>0) {

    results_lists_ok <- lapply(results_lists_ok, function(r){

      # !!!!! If one or more simulation reps returns an incorrectly-formatted
      #       list, r$results will be NULL and there will be an uncaught error.
      #       simba should return `results_lists_ok` so that the user can
      #       inspect it. Fix this when coding GitHub issue #21

      c("sim_uid"=r$sim_uid, "runtime"=r$runtime, r$results)

    })
    results_df <- data.table::rbindlist(results_lists_ok)

  }

  # Convert errors to data frame
  if (num_err>0) {

    results_lists_err <- lapply(results_lists_err, function(r){
      list("sim_uid" = r$sim_uid,
           "runtime" = r$runtime,
           "message" = r$results$message,
           "call" = ifelse(is.null(r$results$call), NA,
                           paste(deparse(r$results$call), collapse="")))
    })
    errors_df <- data.table::rbindlist(results_lists_err)

  }

  # Convert warnings to data frame
  if (num_warn>0) {

    results_lists_warn <- lapply(results_lists_warn, function(r){
      list("sim_uid" = r$sim_uid,
           "runtime" = r$runtime,
           "message" = paste(r$warnings, collapse="; "))
    })
    warn_df <- data.table::rbindlist(results_lists_warn)

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

  # Join error data frames with `levels_grid_big`and attach to sim_obj
  if (exists("errors_df")) {
    errors_df <- dplyr::inner_join(
      sim_obj$internals$levels_grid_big,
      errors_df,
      by = "sim_uid"
    )
    sim_obj$errors <- errors_df
  }

  # Join warnings data frames with `levels_grid_big`and attach to sim_obj
  if (exists("warn_df")) {
    warn_df <- dplyr::inner_join(
      sim_obj$internals$levels_grid_big,
      warn_df,
      by = "sim_uid"
    )
    sim_obj$warnings <- warn_df
  } else {
    sim_obj$warnings <- "No warnings"
  }

  # Set states
  if (exists("results_df") && exists("errors_df")) {
    sim_obj$internals$run_state <- "run, some errors"
  } else if (exists("results_df")) {
    sim_obj$internals$run_state <- "run, no errors"
    sim_obj$errors <- "No errors"
  } else if (exists("errors_df")) {
    sim_obj$internals$run_state <- "run, all errors"
    sim_obj$results <- "Errors detected in 100% of simulation replicates"
  } else {
    stop("An unknown error occurred")
  }

  cat(comp_msg)

  sim_obj$internals$end_time <- Sys.time()
  sim_obj$internals$total_runtime <- as.numeric(
    difftime(sim_obj$internals$end_time, sim_obj$internals$start_time),
    units = "secs"
  )

  # record levels and num_sim that were run
  sim_obj$internals$levels_prev <- sim_obj$internals$levels_shallow
  sim_obj$internals$num_sim_prev <- sim_obj$config$num_sim

  return (sim_obj)

}
