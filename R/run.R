#' Run the simulation
#'
#' @description This is the workhorse function of \pkg{SimEngine} that actually
#'     runs the simulation. This should be called after all functions that set
#'     up the simulation (\code{set_config}, \code{set_script}, etc.) have been
#'     called.
#' @param sim A simulation object of class \code{sim_obj}, usually created by
#'     \code{\link{new_sim}}
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
run <- function(sim) {
  UseMethod("run")
}

#' @export
run.sim_obj <- function(sim) {

  sim$vars$start_time <- Sys.time()

  # Set up parallelization code
  if (sim$config$parallel %in% c("inner", "outer")) {

    n_available_cores <- parallel::detectCores()
    if (is.na(sim$config$n_cores)) {
      sim$config$n_cores <- n_available_cores - 1
    } else {
      if (sim$config$n_cores>n_available_cores) {
        warning(paste(sim$config$n_cores, "cores requested but only",
                      n_available_cores, "cores available. Proceeding with",
                      n_available_cores, "cores."))
        sim$config$n_cores <- n_available_cores
      }
    }

    # Create cluster and export everything in env
    cl <- parallel::makeCluster(sim$config$n_cores)
    parallel::clusterExport(cl, ls(sim$vars$env, all.names=T), sim$vars$env)
    ..packages <- c(sim$config$packages, "magrittr")
    parallel::clusterExport(cl, c("sim","..packages"), environment())
    parallel::clusterExport(cl, c("..env"), .GlobalEnv)
    parallel::clusterCall(cl, function(x) {.libPaths(x)}, .libPaths())
    parallel::clusterEvalQ(cl, sapply(..packages, function(p) {
      do.call("library", list(p))
    }))

  } else if (sim$config$parallel=="none") {

    sim$config$n_cores <- 1

  } else if (sim$config$parallel=="cluster") {

    if (is.na(sim$config$n_cores)) {
      sim$config$n_cores <- sim$vars$num_sim_total # !!!!! Will break with new update() code
      assign(x="..flag_batch_n_cores", value=T, envir=sim$vars$env)
    }

  }

  if (!sim$internals$update_sim) {
    sim$internals$levels_grid_big <- create_levels_grid_big(sim)
    sim$internals$num_batches <- max(sim$internals$levels_grid_big$batch_id)
  }

  run_script <- function(core_id) {

    sim_uids <- sim$internals$levels_grid_big$sim_uid[
      sim$internals$levels_grid_big$core_id==core_id
    ]

    res <- lapply(sim_uids, function(i) {

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
      for (obj_name in ls(sim$vars$env, all.names=T)) {
        obj <- get(obj_name, envir=sim$vars$env, inherits=FALSE)
        if (methods::is(obj,"function") && !is.null(environment(obj))) {
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
      # Use withCallingHandlers to catch warnings and tryCatch to catch errors
      .gotWarnings <- character(0) # holds the warnings
      .catch_errors_and_warnings <- as.logical(
        sim$config$stop_at_error==FALSE || Sys.getenv("sim_run")!=""
      )
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
        script_results <- do.call(what="..script", args=list(),
                                  envir=sim$vars$env)
      }

      runtime <- as.numeric(difftime(Sys.time(), ..start_time), units="secs")

      return(list(
        "sim_uid" = i,
        "runtime" = runtime,
        "results" = script_results,
        "warnings" = .gotWarnings
      ))

    })

    return(res)

  }

  # Set up progress bar
  if (sim$config$progress_bar) {
    pbapply::pboptions(type="txt", char="#", txt.width=40, style=3)
  } else {
    pbapply::pboptions(type="none")
  }

  # Set core_ids based on whether sims are running on cluster
  if (sim$config$parallel=="cluster") {
    core_ids <- sim$internals$tid
    if (core_ids>sim$config$n_cores) {
      stop(paste0("This simulation has n_cores=", sim$config$n_cores,
                  ", so this core will not be used."))
    }
    if (!sim$internals$update_sim && core_ids>sim$internals$num_batches) {
      stop(paste0("This simulation only contains ", sim$internals$num_batches,
                  " replicate batches, so this core will not be used."))
    }
  } else {
    core_ids <- c(1:max(sim$internals$levels_grid_big$core_id))
  }

  # Run simulations
  if (sim$config$parallel=="outer") {
    # Run in parallel
    results_lists <- pbapply::pblapply(core_ids, run_script, cl=cl)
  } else {
    # Run serially
    results_lists <- pbapply::pblapply(core_ids, run_script)
  }

  # Combine lists
  results_lists <- unlist(results_lists, recursive=F)

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
    results_df$batch_id <- NULL
    results_df$core_id <- NULL
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
    errors_df$batch_id <- NULL
    errors_df$core_id <- NULL
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
    warn_df$batch_id <- NULL
    warn_df$core_id <- NULL
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

  # Record levels and num_sim that were run
  sim$internals$levels_prev <- sim$internals$levels_shallow

  # Remove global L if it was created
  suppressWarnings( rm("L", envir=.GlobalEnv) )

  # Clear the batch_cache
  assign(x="..batch_cache", value=new.env(), envir=sim$vars$env)
  assign(x="batch_levels", value=NA,
         envir=get(x="..batch_cache", envir=sim$vars$env))

  return (sim)

}
