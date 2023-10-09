#' Framework for updating simulations on a cluster computing system
#'
#' @noRd
cluster_execute <- function(
  first, main, last, cluster_config, keep_errors=T, update_switch=F
) {

  # Capture current working directory and reset it on function exit
  ..oldwd <- getwd()
  on.exit(setwd(..oldwd))

  # Error handling
  handle_errors(keep_errors, "is.boolean")

  # Alias cluster_configvariable
  ..cfg <- cluster_config
  rm(cluster_config)

  # Helper function to add objects in calling envir to the sim_obj envir
  .add_objs <- function(..env_calling, env_sim) {
    for (obj_name in ls(..env_calling, all.names=T)) {
      obj <- get(x=obj_name, envir=..env_calling)
      if (!methods::is(obj,"sim_obj") && obj_name!="L") {
        assign(x=obj_name, value=obj, envir=env_sim)
      }
    }
  }

  # Get a reference to the calling environment
  ..env_calling <- parent.frame(n=2)

  # Run all code locally if simulation is not being run on cluster
  if (!running_on_ccs()) {

    eval(first, envir=..env_calling)
    eval(main, envir=..env_calling)
    eval(last, envir=..env_calling)

  } else {

    # Construct necessary paths
    ..path_sim_obj <- "sim.rds"
    ..path_sim_res <- "sim_results"

    # Error handling: incorrect Sys.getenv("run") variable
    if (!(Sys.getenv("sim_run") %in% c("first", "main", "last"))) {
      stop(paste("The 'sim_run' environment variable must equal either",
                 "'first', 'main', or 'last'."))
    }

  }

  # FIRST: Run 'first' code or return existing simulation object
  if (Sys.getenv("sim_run")=="first") {

    # Test whether cfg$dir is a valid directory
    if (!is.null(..cfg$dir) && !dir.exists(..cfg$dir)) {
      stop(paste("Directory", ..cfg$dir, "does not exist."))
    }

    # Test whether we can write to cfg$dir
    ..test_file <- paste0(..path_sim_obj, '.test')
    tryCatch(
      expr = { saveRDS(list(a=123,b=456), file=..test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not writable."))
      }
    )

    # Test whether we can read from cfg$dir
    tryCatch(
      expr = { x <- readRDS(file=..test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not readable."))
      }
    )

    # Test whether we can delete from cfg$dir
    tryCatch(
      expr = { unlink(..test_file) },
      error = function(e) {
        stop(paste0("Files cannot be deleted from directory ", ..cfg$dir, "."))
      }
    )

    # Set working directory
    if (!is.null(..cfg$dir)) { setwd(..cfg$dir) }

    # Remove old files
    if (dir.exists(..path_sim_res)) { unlink(..path_sim_res, recursive=TRUE) }

    # Create directory to store simulation results
    dir.create(..path_sim_res)

    # Run code (`first` block)
    ..start_time <- Sys.time()
    eval(first, envir=..env_calling)

    # Extract the simulation object variable name
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env_calling, all.names=T)) {
      if (methods::is(get(x=obj_name, envir=..env_calling), "sim_obj")) {
        ..sim_var <- obj_name
        ..count <- ..count + 1
      }
    }
    if (is.na(..sim_var)) {
      if (update_switch) {
        stop("A simulation object must be loaded in the `first` block")
      } else {
        stop("A simulation object must be created in the `first` block")
      }

    }
    if (..count>1) {
      stop(paste("Multiple simulation objects were detected; only one may be",
                 "read in the `first` block"))
    }
    rm(..count)

    # Get reference to simulation object
    ..sim <- get(..sim_var, envir=..env_calling)
    .add_objs(..env_calling, ..sim$vars$env)

    # Save simulation object
    ..sim$internals$sim_var <- ..sim_var
    ..sim$vars$start_time <- ..start_time
    ..sim$config$parallel <- TRUE
    saveRDS(..sim, file=..path_sim_obj)

  } else if (Sys.getenv("sim_run") %in% c("main","last")) {

    # Set working directory
    if (!is.null(..cfg$dir)) { setwd(..cfg$dir) }

    tryCatch(
      ..sim <- readRDS(..path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object was not found. Make sure your 'first' function",
          "is not producing errors and returns a valid simulation object, and",
          "that your shell commands are correct and properly sequenced."))
      }
    )

    handle_errors(..sim, "is.sim_obj")

    # Create hidden variable references
    ..e <- .GlobalEnv
    assign(x="..env", value=..sim$vars$env, envir=..e)

  }

  # MAIN: run simulation replicate and save results/errors
  if (Sys.getenv("sim_run")=="main") {

    # If there are error files in the results directory and stop_at_error==T,
    #     skip this rep
    err_reps <- list.files(path=..path_sim_res, pattern="e_*")
    if (!(length(err_reps)>0 && ..sim$config$stop_at_error)) {

      # Error handling: tid_var and js
      if (is.null(..cfg$tid_var) && is.null(..cfg$js)) {
        stop("You must specify either 'js' or 'tid_var' in cluster_config.")
      }
      if (!is.null(..cfg$tid_var) && !is.null(..cfg$js)) {
        warning(paste0("Both 'js' and 'tid_var' were specified in cluster_conf",
                       "ig; js will be ignored."))
      }

      if (!is.null(..cfg$tid_var)) {
        tid_var <- ..cfg$tid_var
      } else if (!is.null(..cfg$js)) {

        # Make 'js' case insensitive
        ..cfg$js <- tolower(..cfg$js)

        if (!(..cfg$js %in% (js_support())$js_code)) {
          stop(paste("cluster_config variable 'js' is invalid; for a list of",
                     "supported job schedulers, run js_support()"))
        }

        tid_var <- js_support()[which(js_support()$js_code==..cfg$js),"tid"]

      }

      tid <- as.numeric(Sys.getenv(tid_var))

      if (is.na(tid)) { stop("Task ID is missing.") }

      add_to_tid <- as.numeric(Sys.getenv("sim_add_to_tid"))
      if (!is.na(add_to_tid)) { tid <- tid + add_to_tid }

      if (tid<1 || tid>..sim$vars$num_sim_total) {

        stop(paste(
          "Task ID is invalid; must be an integer between 1 and",
          ..sim$vars$num_sim_total
        ))

      } else {

        # Assign tid, load packages, and run 'main' code
        ..sim$internals$tid <- tid
        rm(tid, add_to_tid)
        for (pkg in ..sim$config$packages) { do.call("library", list(pkg)) }
        assign(..sim$internals$sim_var, ..sim, envir=..env_calling)
        eval(main, envir=..env_calling)
        ..sim <- get(..sim$internals$sim_var, envir=..env_calling)

      }

      # Save results/errors/warnings
      fmt <- paste0("%0", nchar(..sim$vars$num_sim_total), "d")
      if (..sim$vars$run_state %in% c("run, no errors", "run, some errors")) {
        saveRDS(
          list("results" = ..sim$results,
               "results_complex" = ..sim$results_complex),
          paste0(..path_sim_res, "/r_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      }
      if (..sim$vars$run_state %in% c("run, all errors",
                                             "run, some errors")) {
        saveRDS(
          ..sim$errors,
          paste0(..path_sim_res, "/e_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      }
      if (!is.character(..sim$warnings)) {
        saveRDS(
          ..sim$warnings,
          paste0(..path_sim_res, "/w_",
                 sprintf(fmt, ..sim$internals$tid), ".rds")
        )
      }

    }
  }

  # LAST: merge results/errors into simulation object, run 'last' code, and save
  if (Sys.getenv("sim_run")=="last") {

    # If there are error files in the results directory and stop_at_error==TRUE,
    #     skip this rep
    err_reps <- list.files(path=..path_sim_res, pattern="e_*")
    if (length(err_reps)>0 & ..sim$config$stop_at_error) {

      unlink(paste0(..path_sim_res, "/r_*"))

    } else {

      # Process result/error files
      files <- dir(..path_sim_res)
      results_df <- NULL
      results_complex <- list()
      errors_df <- NULL
      warnings_df <- NULL

      for (file in files) {

        if (substr(file,1,1)=="r") {

          r <- readRDS(paste0(..path_sim_res, "/", file))
          if (is.null(results_df)) {
            results_df <- r$results
          } else {
            results_df <- rbind(results_df, r$results)
          }
          if (length(r$results_complex)>0) {
            results_complex <- c(results_complex, r$results_complex)
          }

        } else if (substr(file,1,1)=="e") {

          e <- readRDS(paste0(..path_sim_res, "/", file))
          if (methods::is(e,"data.frame")) {
            if (is.null(errors_df)) {
              errors_df <- e
            } else {
              errors_df <- rbind(errors_df, e)
            }
          }

        } else if (substr(file,1,1) == "w") {

          w <- readRDS(paste0(..path_sim_res, "/", file))
          if (methods::is(w,"data.frame")) {
            if (is.null(warnings_df)) {
              warnings_df <- w
            } else {
              warnings_df <- rbind(warnings_df, w)
            }
          }

        }
      }

      if (!update_switch) {

        ..sim$results_complex <- results_complex

        if (!is.null(results_df)) {
          ..sim$results <- results_df
        } else {
          ..sim$results <- "Errors detected in 100% of simulation replicates"
        }

        if (!is.null(errors_df)) {
          ..sim$errors <- errors_df
        } else {
          ..sim$errors <- "No errors"
        }

        if (!is.null(warnings_df)) {
          ..sim$warnings <- warnings_df
        } else {
          ..sim$warnings <- "No warnings"
        }

      } else {

        # Remove inactive results/errors/warnings
        ..sim <- delete_inactive_rwe(..sim)

        # Combine results/errors/warnings of original run and updated run
        ..sim <- combine_original_with_update(
          sim = ..sim,
          results_new = results_df,
          results_complex_new = results_complex,
          errors_new = errors_df,
          warnings_new = warnings_df
        )

      }

      # Update run_state variable
      ..sim$vars$run_state <- update_run_state(..sim)

      # Reset sim_uid_grid$to_run
      ..sim$internals$sim_uid_grid$to_run <- F

      # Delete individual results files and save simulation object
      # This is done before running the 'last' code so that the compiled
      #     simulation object is saved even if there's an error with the 'last'
      #     code
      # Note: the for-loop helps get around a bug related to file-locking
      for (i in 1:5) {
        x <- unlink(..path_sim_res, recursive=TRUE)
        if (x == 0) { break }
        Sys.sleep(0.2)
      }
      saveRDS(..sim, file=..path_sim_obj)

      # Run 'last' code
      assign(..sim$internals$sim_var, ..sim, envir=..env_calling)
      for (pkg in ..sim$config$packages) { do.call("library", list(pkg)) }
      eval(last, envir=..env_calling)
      ..sim <- get(..sim$internals$sim_var, envir=..env_calling)
      .add_objs(..env_calling, ..sim$vars$env)

      # Save final simulation object (a second time, if 'last' code had no
      #     errors)
      ..sim$vars$end_time <- Sys.time()
      ..sim$vars$total_runtime <- as.numeric(
        difftime(..sim$vars$end_time, ..sim$vars$start_time),
        units = "secs"
      )
      saveRDS(..sim, file=..path_sim_obj)
    }

  }

}
