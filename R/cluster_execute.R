#' Framework for updating simulations on a cluster computing system
#'
#' @param first Code to run at the start of a simulation. This should be a block
#'     of code enclosed by curly braces {} that that creates a simulation
#'     object. Put everything you need in the simulation object, since global
#'     variables declared in this block will not be available when the 'main'
#'     and 'last' code blocks run.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {} that includes a call to
#'     \link{run}. This code block will have access to the simulation object you
#'     created in the 'first' code block, but any changes made here to the
#'     simulation object will not be saved.
#' @param last Code that will run after all simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that takes
#'     your simulation object (which at this point will contain your results)
#'     and do something with it, such as display your results on a graph.
#' @param cluster_config A list of configuration options. You must specify
#'     either js (the job scheduler you are using) or tid_var (the name of the
#'     environment variable that your task ID) is stored in. You can optionally
#'     specify dir, which is a path to a directory that will hold your
#'     simulation object and results (this defaults to the current working
#'     directory).
#' @noRd
cluster_execute <- function(first,
                            main,
                            last,
                            cluster_config,
                            keep_errors = TRUE,
                            keep_extra = FALSE,
                            update_switch = FALSE) {

  # error handle invalid options
  handle_errors(keep_errors, "is.boolean")
  handle_errors(keep_extra, "is.boolean")
  handle_errors(update_switch, "is.boolean")

  # Rename arguments to reduce changes of a naming conflict with contents of
  #   first/main/last blocks
  ..first <- first
  ..main <- main
  ..last <- last
  ..cfg <- cluster_config
  rm(first)
  rm(main)
  rm(last)
  rm(cluster_config)

  # Run all code locally if simulation is not being run on cluster
  # !!!!! TO-DO make this work for update_switch = TRUE
  if (Sys.getenv("simba_run")=="") {

    # Run code locally (`first` block)
    eval(..first)

    # Extract the simulation object variable name
    ..env <- environment()
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env)) {
      if ("simba" %in% class(get(x=obj_name, envir=..env))) {
        ..sim_var <- obj_name
        ..count <- ..count + 1
      }
    }
    if (is.na(..sim_var)) {
      stop("A simulation object must be created in the `first` block")
    }
    if (..count>1) {
      stop(paste("Multiple simulation objects were detected; only one may be",
                 "created in the `first` block"))
    }
    rm(..count)
    rm(..env)

    # Run code locally (`main` and `last` blocks)
    eval(..main)
    eval(..last)

    # Assign simulation object to ..sim_var in the parent environment
    assign(
      x = ..sim_var,
      value = eval(as.name(..sim_var)),
      envir = parent.frame(n = 2)
    )

  } else {

    # Construct necessary paths
    if (is.null(..cfg$dir)) {
      ..path_sim_obj <- "sim.simba"
      ..path_sim_out <- "sim_output.txt"
      ..path_sim_res <- "simba_results"
    } else {
      ..path_sim_obj <- paste0(..cfg$dir, "/sim.simba")
      ..path_sim_out <- paste0(..cfg$dir, "/sim_output.txt")
      ..path_sim_res <- paste0(..cfg$dir, "/simba_results")
    }

    # Error handling: incorrect Sys.getenv("run") variable
    if (!(Sys.getenv("simba_run") %in% c("first", "main", "last"))) {
      stop(paste("The 'simba_run' environment variable must equal either",
                 "'first', 'main', or 'last'."))
    }

  }

  # FIRST: Run 'first' code or return existing simulation object
  if (Sys.getenv("simba_run")=="first") {

    # Check that cfg$dir is a valid directory
    if (!is.null(..cfg$dir) && !dir.exists(..cfg$dir)) {
      stop(paste("Directory", ..cfg$dir, "does not exist."))
    }

    # !!!!! changed this to not erase the sim object
    test_file <- paste0(..path_sim_obj, '.test')
    # Error handling: test to see that we can write to cfg$dir
    tryCatch(
      expr = { saveRDS(list(a=123,b=456), file=test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not writable."))
      }
    )

    # Error handling: test to see that we can read from cfg$dir
    tryCatch(
      expr = { x <- readRDS(file=test_file) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not readable."))
      }
    )

    # Error handling: test to see that we can delete from cfg$dir
    tryCatch(
      expr = { unlink(test_file) },
      error = function(e) {
        stop(paste0("Files cannot be deleted from directory ", ..cfg$dir, "."))
      }
    )

    # Remove old files
    if (dir.exists(..path_sim_res)) { unlink(..path_sim_res, recursive=TRUE) }

    # Create directory to store simulation results
    dir.create(..path_sim_res)

    ..start_time <- Sys.time()

    # Run 'first' code
    eval(..first)

    # Extract the simulation object variable name
    ..env <- environment()
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env)) {
      if ("simba" %in% class(get(x=obj_name, envir=..env))) {
        ..sim_var <- obj_name
        ..count <- ..count + 1
      }
    }
    if (is.na(..sim_var)) {
      if (update_switch) {
        stop("A simulation object must be read in the `first` block")
      } else {
        stop("A simulation object must be created in the `first` block")
      }

    }
    if (..count>1) {
      stop(paste("Multiple simulation objects were detected; only one may be",
                 "read in the `first` block"))
    }
    rm(..count)
    rm(..env)

    # Save simulation object
    # We assume the user doesn't name their simulation object '..sim_obj'
    ..sim_obj <- eval(as.name(..sim_var))
    ..sim_obj$internals$sim_var <- ..sim_var
    ..sim_obj$vars$start_time <- ..start_time
    ..sim_obj$config$parallel <- "none" # !!!!! Revisit this
    saveRDS(..sim_obj, file=..path_sim_obj)

  } else if (Sys.getenv("simba_run") %in% c("main","last")) {

    tryCatch(
      ..sim_obj <- readRDS(..path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object was not found. Make sure your 'first' function",
          "is not producing errors and returns a valid simulation object, and",
          "that your shell commands are properly sequenced."))
      }
    )
    #print(..sim_obj$internals)

    if (!class(..sim_obj)=="simba") {
      stop("Invalid simulation object")
    }

  }

  # MAIN: run simulation replicate and save results/errors
  if (Sys.getenv("simba_run")=="main") {

    # if there are error files in the results directory and stop_at_error is TRUE
    # skip this rep
    err_reps <- list.files(path = ..path_sim_res, pattern = "e_*")
    if (!(length(err_reps) > 0 & ..sim_obj$config$stop_at_error)){

      # Error handling: tid_var and js
      if (is.null(..cfg$tid_var) && is.null(..cfg$js)) {
        stop("You must specify either 'js' or 'tid_var' in cluster_config.")
      }

      # User specified cfg$tid_var
      if (!is.null(..cfg$tid_var)) {
        tid_var <- ..cfg$tid_var
      }

      # User specified cfg$js
      if (!is.null(..cfg$js)) {

        # Make 'js' case insensitive
        ..cfg$js <- tolower(..cfg$js)

        if (!(..cfg$js %in% c("slurm","sge"))) {
          stop(paste(
            "cluster_config variable 'js' must equal one of the following:",
            "'slurm', 'sge'."))
        }

        tid_var <- dplyr::case_when(
          ..cfg$js=="slurm" ~ "SLURM_ARRAY_TASK_ID",
          ..cfg$js=="sge" ~ "SGE_TASK_ID"
        )

      }

      tid <- as.numeric(Sys.getenv(tid_var))

      if (is.na(tid)) {
        stop("Task ID is missing.")
      }

      add_to_tid <- as.numeric(Sys.getenv("simba_run_add_to_tid"))
      if (!is.na(add_to_tid)) {
        tid <- tid + add_to_tid
      }

      if (tid<1 || tid>..sim_obj$vars$num_sim_total) {
        stop(paste(
          "Task ID is invalid; must be an integer between 1 and",
          ..sim_obj$vars$num_sim_total
        ))
      } else {
        if (update_switch) {
          # for updating, need to add number of previously run sims (num_sim_cumulative)
          tid <- tid + ..sim_obj$internals$num_sim_cumulative
        }
        # Run 'main' code
        ..sim_obj$internals$tid <- tid
        rm(tid)
        rm(add_to_tid)
        for (pkg in ..sim_obj$config$packages) {
          do.call("library", list(pkg))
        }
        assign(..sim_obj$internals$sim_var, ..sim_obj)
        eval(..main)
        assign("..sim_obj", eval(as.name(..sim_obj$internals$sim_var)))
      }

      # Parse results filename and save
      fmt <- paste0("%0", nchar(..sim_obj$vars$num_sim_total), "d")

      if (..sim_obj$vars$run_state=="run, no errors") {
        saveRDS(
          list(
            "results" = ..sim_obj$results,
            "results_complex" = ..sim_obj$results_complex
          ),
          paste0(..path_sim_res, "/r_",
                 sprintf(fmt, ..sim_obj$internals$tid), ".rds")
        )
      } else if (..sim_obj$vars$run_state=="run, all errors") {
        saveRDS(
          ..sim_obj$errors,
          paste0(..path_sim_res, "/e_",
                 sprintf(fmt, ..sim_obj$internals$tid), ".rds")
        )
      }
      if (!is.character(..sim_obj$warnings)) {
        saveRDS(
          ..sim_obj$warnings,
          paste0(..path_sim_res, "/w_",
                 sprintf(fmt, ..sim_obj$internals$tid), ".rds")
        )
      }
    }
  }

  # LAST: merge results/errors into simulation object, run 'last' code, and save
  if (Sys.getenv("simba_run")=="last") {

    # if there are error files in the results directory and stop_at_error is TRUE
    # skip this rep
    err_reps <- list.files(path = ..path_sim_res, pattern = "e_*")
    if (length(err_reps) > 0 & ..sim_obj$config$stop_at_error){
      ..f <- file(..path_sim_out, open="wt")
      sink(..f, type="output", append=FALSE)
      sink(..f, type="message", append=FALSE)
      cat(paste("simba output START:",Sys.time(),"\n\n"))
      cat(paste("\nThe simluation was stopped because of an error. See error files in simba_results directory.\n\n"))
      cat(paste("\n\nsimba output END:",Sys.time(),"\n"))
      sink(type="output")
      sink(type="message")
      close(..f)

      unlink(paste0(..path_sim_res, "/r_*"))
    } else {
      # Process result/error files
      files <- dir(paste0(..path_sim_res))
      results_df <- NULL
      results_complex <- list()
      errors_df <- NULL
      warnings_df <- NULL
      num_new <- 0
      for (file in files) {

        if (substr(file,1,1)=="r") {

          r <- readRDS(paste0(..path_sim_res, "/", file))

          if (is.null(results_df)) {
            results_df <- r$results
          } else {
            results_df[nrow(results_df)+1,] <- r$results
          }

          if (!is.na(r$results_complex)) {
            results_complex[[length(results_complex)+1]] <-
              r$results_complex[[1]]
          }

          num_new <- num_new + 1

        } else if (substr(file,1,1)=="e") {

          e <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(e)=="data.frame") {
            if (is.null(errors_df)) {
              errors_df <- e
            } else {
              errors_df[nrow(errors_df)+1,] <- e
            }
          }

          num_new <- num_new + 1

        } else if (substr(file,1,1) == "w") {
          w <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(w)=="data.frame") {
            if (is.null(warnings_df)) {
              warnings_df <- w
            } else {
              warnings_df[nrow(warnings_df)+1,] <- w
            }
          }
        }
      }

      if (identical(results_complex,list())) {
        results_complex <- NA
      }

      if (update_switch) {
        # combine results and errors with existing results and errors
        if (!is.character(..sim_obj$results)) {
          results_df <- rbind(..sim_obj$results, results_df)
          results_df <- results_df[order(results_df$sim_uid),]
        }
        if (!is.na(results_complex)) {
          results_complex <- c(..sim_obj$results_complex, results_complex)
        }
        if (!is.character(..sim_obj$errors)) {
          errors_df <- rbind(..sim_obj$errors, errors_df)
          errors_df <- errors_df[order(errors_df$sim_uid),]
        }
        if (!is.character(..sim_obj$warnings)) {
          warnings_df <- rbind(..sim_obj$warnings, warnings_df)
          warnings_df <- warnings_df[order(warnings_df$sim_uid),]
        }
      }

      # Add results/errors to simulation object
      # Note: this code is somewhat redundant with the end of simba::run()
      if (!is.null(warnings_df)) {
        ..sim_obj$warnings <- warnings_df
      }
      else {
        ..sim_obj$warnings <- "No warnings"
      }
      if (!is.null(results_df) && !is.null(errors_df)) {
        ..sim_obj$results <- results_df
        ..sim_obj$results_complex <- results_complex
        ..sim_obj$errors <- errors_df
        ..sim_obj$vars$run_state <- "run, some errors"
      } else if (!is.null(results_df)) {
        ..sim_obj$results <- results_df
        ..sim_obj$results_complex <- results_complex
        ..sim_obj$errors <- "No errors"
        ..sim_obj$vars$run_state <- "run, no errors"
      } else if (!is.null(errors_df)) {
        ..sim_obj$results <- "Errors detected in 100% of simulation replicates"
        ..sim_obj$errors <- errors_df
        ..sim_obj$vars$run_state <- "run, all errors"
      } else {
        stop("An unknown error occurred.")
      }

      levels_grid_big <- create_levels_grid_big(..sim_obj)

      if (update_switch) {
        prev_levels_grid_big <- ..sim_obj$internals$levels_grid_big

        # get levels / sim_ids that were previously run but are no longer needed
        extra_run <- dplyr::anti_join(prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in% c("sim_uid", "level_id")),drop=F],
                                      levels_grid_big[,-which(names(levels_grid_big) %in% c("sim_uid", "level_id")),drop=F],
                                      by = names(prev_levels_grid_big[,-which(names(prev_levels_grid_big) %in% c("sim_uid", "level_id")),drop=F]))

        # if keep_extra = FALSE, remove excess runs (from results, errors, and warnings)
        if (!keep_extra & nrow(extra_run) > 0){

          if (!is.character(..sim_obj$results)){
            ..sim_obj$results <- dplyr::anti_join(..sim_obj$results,
                                                  extra_run,
                                                  by = names(extra_run))
          }
          if (!is.character(..sim_obj$errors)){
            ..sim_obj$errors <- dplyr::anti_join(..sim_obj$errors,
                                                 extra_run,
                                                 by = names(extra_run))
          }
          if (!is.character(..sim_obj$warnings)){
            ..sim_obj$warnings <- dplyr::anti_join(..sim_obj$warnings,
                                                   extra_run,
                                                   by = names(extra_run))
          }
        }
      }

      # record levels and num_sim that were run
      ..sim_obj$internals$levels_prev <- ..sim_obj$internals$levels_shallow
      ..sim_obj$internals$num_sim_prev <- ..sim_obj$config$num_sim
      ..sim_obj$internals$levels_grid_big <- levels_grid_big

      if (update_switch) {
        ..sim_obj$internals$update_sim <- TRUE
        ..sim_obj$internals$num_sim_cumulative <- ..sim_obj$internals$num_sim_cumulative + num_new
      } else {
        ..sim_obj$internals$num_sim_cumulative <- ..sim_obj$internals$num_sim_cumulative + ..sim_obj$vars$num_sim_total
      }

      # Delete individual results files and save simulation object
      # This is done before running the 'last' code so that the compiled
      #   simulation object is saved even if there's an error with the 'last'
      #   code
      unlink(..path_sim_res, recursive=TRUE)
      saveRDS(..sim_obj, file=..path_sim_obj)

      # Run 'last' code
      # Divert output to simba_output.txt file
      ..f <- file(..path_sim_out, open="wt")
      sink(..f, type="output", append=FALSE)
      sink(..f, type="message", append=FALSE)
      cat(paste("simba output START:",Sys.time(),"\n\n"))
      for (pkg in ..sim_obj$config$packages) {
        do.call("library", list(pkg))
      }
      assign(..sim_obj$internals$sim_var, ..sim_obj)
      eval(..last)
      cat(paste("\n\nsimba output END:",Sys.time(),"\n"))
      sink(type="output")
      sink(type="message")
      close(..f)

      # Save final simulation object (a second time, if 'last' code had no errors)
      assign("..sim_obj", eval(as.name(..sim_obj$internals$sim_var)))
      ..sim_obj$vars$end_time <- Sys.time()
      ..sim_obj$vars$total_runtime <- as.numeric(
        difftime(..sim_obj$vars$end_time, ..sim_obj$vars$start_time),
        units = "secs"
      )
      saveRDS(..sim_obj, file=..path_sim_obj)
    }

  }

}
