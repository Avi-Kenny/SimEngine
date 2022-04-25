#' Run the simulation
#'
#' @description This is the workhorse function of \pkg{SimEngine} that actually
#'     runs the simulation. This should be called after all functions that set
#'     up the simulation (\code{set_config}, \code{set_script}, etc.) have been
#'     called.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
#' @param sim_uids Advanced; a vector of \code{sim_uid} values, each of which
#'     uniquely identifies a simulation replicate. This will normally be
#'     omitted. If this is specified, only the simulation replicates with a
#'     matching \code{sim_uid} will be run.
#' @return The original simulation object but with the results attached (along
#'     with any errors and warnings). Results are stored in \code{sim$results},
#'     errors are stored in \code{sim$errors}, and warnings are stored in
#'     \code{sim$warnings}.
#' @examples
#' # The following is a toy example of a simulation, illustrating the use of
#' # the run function.
#' sim <- new_sim()
#' create_data <- function(n) { rpois(n, lambda=5) }
#' est_mean <- function(dat, type) {
#'   if (type=="M") { return(mean(dat)) }
#'   if (type=="V") { return(var(dat)) }
#' }
#' sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
#' sim %<>% set_config(num_sim=1)
#' sim %<>% set_script(function() {
#'   dat <- create_data(L$n)
#'   lambda_hat <- est_mean(dat=dat, type=L$est)
#'   return (list("lambda_hat"=lambda_hat))
#' })
#' sim %<>% run()
#' sim$results %>% print()
#' @export
run <- function(sim, sim_uids=NA) UseMethod("run")

#' @export
run.sim_obj <- function(sim, sim_uids=NA) {

  handle_errors(sim, "is.sim_obj")

  sim$vars$start_time <- Sys.time()

  if (is.na(sim_uids)) {
  # !!!!! add error handling for sim_uids
    if (!is.na(sim$internals$tid)) {
      sim_uids <- sim$internals$tid
    } else if (sim$internals$update_sim) {
      sim_uids <- sim$internals$levels_grid_big$sim_uid
    } else {
      sim_uids <- 1:sim$vars$num_sim_total
    }
  }

  if (!sim$internals$update_sim) {
    # Create levels_grid_big
    levels_grid_big <- create_levels_grid_big(sim)
    sim$internals$levels_grid_big <- levels_grid_big
  }

  # Set up parallelization code
  if (sim$config$parallel %in% c("inner", "outer")) {

    ..packages <- c(sim$config$packages, "magrittr")
    n_available_cores <- parallel::detectCores()
    if (sim$config$n_cores==0) {
      n_cores <- n_available_cores - 1
    } else {
      if (sim$config$n_cores>n_available_cores) {
        n_cores <- n_available_cores
        warning(paste(sim$config$n_cores, "cores requested but only",
                      n_available_cores, "cores available. Proceeding with",
                      n_available_cores, "cores."))
      } else {
        n_cores <- sim$config$n_cores
      }
    }

    # Create cluster and export everything in env
    cl <- parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, ls(sim$vars$env), sim$vars$env)
    parallel::clusterExport(cl, c("sim","..packages"), environment())
    parallel::clusterExport(cl, "..env", .GlobalEnv)
    parallel::clusterCall(cl, function(x) {.libPaths(x)}, .libPaths())
    parallel::clusterEvalQ(cl, sapply(..packages, function(p) {
      do.call("library", list(p))
    }))
  }

  run_script <- function(i) {

    ..start_time <- Sys.time()

    # Set up references to levels row (L)
    L <- as.list(sim$internals$levels_grid_big[
      sim$internals$levels_grid_big$sim_uid == i,
    ])
    levs <- names(sim$levels)
    for (j in 1:length(levs)) {
      # Handle list-type levels
      if (sim$internals$levels_types[j]) {
        L[[levs[j]]] <- sim$levels[[levs[j]]][[L[[levs[j]]]]]
      }
    }
    for (obj_name in ls(sim$vars$env)) {
      obj <- get(obj_name, envir=sim$vars$env, inherits=FALSE)
      if (methods::is(obj,"function")) {
        assign(x="L", value=L, envir=environment(obj))
      }
    }
    assign(x="L", value=L, envir=sim$vars$env)
    rm(levs)
    rm(L)

    # Set the seed
    set.seed(sim$config$seed)
    set.seed(as.integer((1e9*runif(i))[i]))

    # Actually run the run
    # Use withCallingHandlers to catch all warnings and tryCatch to catch errors
    .gotWarnings <- character(0) # holds the warnings
    .catch_errors_and_warnings <- as.logical(sim$config$stop_at_error==FALSE ||
                                               Sys.getenv("sim_run")!="")
    if (.catch_errors_and_warnings) {
      withCallingHandlers(
        expr = {
          script_results <- tryCatch(
            expr = do.call(what="..script", args=list(), envir=sim$vars$env),
            error = function(e) { return(e) }
          )
        },
        warning = function(w) {
          .gotWarnings <<- c(.gotWarnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    } else {
      script_results <- do.call(what="..script", args=list(), envir=sim$vars$env)
    }

    runtime <- as.numeric(difftime(Sys.time(), ..start_time), units="secs")

    return (list(
      "sim_uid" = i,
      "runtime" = runtime,
      "results" = script_results,
      "warnings" = .gotWarnings
    ))

  }

  # Set up progress bar
  if (sim$config$progress_bar) {
    pbapply::pboptions(type="txt", char="#", txt.width=40, style=3)
  } else {
    pbapply::pboptions(type="none")
  }

  # Run simulations
  if (sim$config$parallel=="outer") {
    # Run in parallel
    results_lists <- pbapply::pblapply(sim_uids, run_script, cl=cl)
  } else {
    # Run serially
    results_lists <- pbapply::pblapply(sim_uids, run_script)
  }

  # Stop cluster
  if (exists("cl")) { parallel::stopCluster(cl) }

  # Separate errors from results
  results_lists_ok <- list()
  results_lists_warn <- list()
  results_lists_err <- list()
  for (i in 1:length(results_lists)) {
    if (methods::is(results_lists[[i]]$results, "error")) {
      results_lists_err[[length(results_lists_err)+1]] <- results_lists[[i]]
    } else {
      results_lists_ok[[length(results_lists_ok)+1]] <- results_lists[[i]]
    }
    if (length(results_lists[[i]]$warnings) > 0) {
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

  # Convert results to data frame and pull out complex data
  if (num_ok>0) {

    # Handle standard results
    results_lists_ok2 <- lapply(results_lists_ok, function(r) {
      r$results$.complex <- NULL
      if (length(r$results)>0) {
        c("sim_uid"=r$sim_uid, "runtime"=r$runtime, r$results)
      } else {
        list("sim_uid"=r$sim_uid, "runtime"=r$runtime)
      }
    })
    results_df <- data.table::rbindlist(results_lists_ok2)

    # Change invalid columns names
    ..rdf_names <- names(results_df)
    ..rdf_names_valid <- make.names(..rdf_names, unique=TRUE)

    if (!identical(..rdf_names,..rdf_names_valid)) {
      names(results_df) <- ..rdf_names_valid
      warning("Some invalid column names were changed.", call.=FALSE)
    }

    # Handle complex results
    if (!is.null(results_lists_ok[[1]]$results$.complex)) {
      r_sim_uids <- results_df$sim_uid
      names(results_lists_ok) <- as.character(paste0("sim_uid_",r_sim_uids))
      results_complex <- lapply(results_lists_ok, function(r) {
        r$results$.complex
      })
      sim$results_complex <- results_complex
    }

    # Join results data frames with `levels_grid_big` and attach to sim
    results_df <- dplyr::inner_join(
      sim$internals$levels_grid_big,
      results_df,
      by = "sim_uid"
    )
    sim$results <- results_df

  }

  # Convert errors to data frame
  if (num_err>0) {

    results_lists_err <- lapply(results_lists_err, function(r) {
      list("sim_uid" = r$sim_uid,
           "runtime" = r$runtime,
           "message" = r$results$message,
           "call" = ifelse(is.null(r$results$call), NA,
                           paste(deparse(r$results$call), collapse="")))
    })
    errors_df <- data.table::rbindlist(results_lists_err)

    # Join error data frames with `levels_grid_big` and attach to sim
    errors_df <- dplyr::inner_join(
      sim$internals$levels_grid_big,
      errors_df,
      by = "sim_uid"
    )
    sim$errors <- errors_df

  }

  # Convert warnings to data frame
  if (num_warn>0) {

    results_lists_warn <- lapply(results_lists_warn, function(r) {
      list("sim_uid" = r$sim_uid,
           "runtime" = r$runtime,
           "message" = paste(r$warnings, collapse="; "))
    })
    warn_df <- data.table::rbindlist(results_lists_warn)

    # Join warnings data frames with `levels_grid_big` and attach to sim
    warn_df <- dplyr::inner_join(
      sim$internals$levels_grid_big,
      warn_df,
      by = "sim_uid"
    )
    sim$warnings <- warn_df

  }

  # Set states
  if (num_warn==0) { sim$warnings <- "No warnings" }
  if (num_ok>0 && num_err>0) {
    sim$vars$run_state <- "run, some errors"
  } else if (num_ok>0) {
    sim$vars$run_state <- "run, no errors"
    sim$errors <- "No errors"
  } else if (num_err>0) {
    sim$vars$run_state <- "run, all errors"
    sim$results <- "Errors detected in 100% of simulation replicates"
  } else {
    stop("An unknown error occurred (CODE 101)")
  }

  message(comp_msg)

  sim$vars$end_time <- Sys.time()
  sim$vars$total_runtime <- as.numeric(
    difftime(sim$vars$end_time, sim$vars$start_time),
    units = "secs"
  )

  # record levels and num_sim that were run
  sim$internals$levels_prev <- sim$internals$levels_shallow
  sim$internals$num_sim_prev <- sim$config$num_sim
  sim$internals$num_sim_cumul <- sim$internals$num_sim_cuml + length(sim_uids)

  # Remove global L if it was created
  suppressWarnings( rm("L", envir=.GlobalEnv) )

  return (sim)

}
