#' Framework for running simulations on a cluster computing system
#'
#' @description !!!!! TO DO. Job schedulers currently supported include Slurm, SGE, ... !!!!!
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
#' @examples
#' !!!!! TO DO
#' @export
run_on_cluster <- function(first, main, last, cluster_config) {

  # Rename arguments to reduce changes of a naming conflict with contents of
  #   first/main/last blocks
  ..first <- substitute(first)
  ..main <- substitute(main)
  ..last <- substitute(last)
  ..cfg <- cluster_config
  rm(first)
  rm(main)
  rm(last)
  rm(cluster_config)

  # Run all code locally if simulation is not being run on cluster
  if (Sys.getenv("run")=="") {

    # Run code locally (`first` block)
    eval(..first)

    # Extract the simulation object variable name
    ..env <- environment()
    ..count <- 0
    ..sim_var <- NA
    for (obj_name in ls(..env)) {
      if (class(base::get(x=obj_name, envir=..env))=="simba") {
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
      envir = parent.frame()
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
    if (!(Sys.getenv("run") %in% c("first", "main", "last"))) {
      stop(paste("The 'run' environment variable must equal either 'first',",
                 "'main', or 'last'."))
    }

  }

  # FIRST: Run 'first' code or return existing simulation object
  if (Sys.getenv("run")=="first") {

    # Check that cfg$dir is a valid directory
    if (!is.null(..cfg$dir) && !dir.exists(..cfg$dir)) {
      stop(paste("Directory", ..cfg$dir, "does not exist."))
    }

    # Error handling: test to see that we can write to cfg$dir
    tryCatch(
      expr = { saveRDS(list(a=123,b=456), file=..path_sim_obj) },
      error = function(e) {
        stop(paste0("Directory ", ..cfg$dir, " is not writable."))
      }
    )

    # Error handling: test to see that we can read from cfg$dir
    tryCatch(
      expr = { x <- readRDS(file=..path_sim_obj) },
      error = function(e) {
        stop(paste0("Directory ", cfg$dir, " is not readable."))
      }
    )

    # Error handling: test to see that we can delete from cfg$dir
    tryCatch(
      expr = { unlink(..path_sim_obj) },
      error = function(e) {
        stop(paste0("Files cannot be deleted from directory ", cfg$dir, "."))
      }
    )

    # Remove old files
    if (dir.exists(..path_sim_res)) { unlink(..path_sim_res, recursive=TRUE) }
    if (file.exists(..path_sim_obj)) { unlink(..path_sim_obj) }

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
      if (class(base::get(x=obj_name, envir=..env))=="simba") {
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

    # Save simulation object
    # We assume the user doesn't name their simulation object '..sim_obj'
    ..sim_obj <- eval(as.name(..sim_var))
    ..sim_obj$internals$sim_var <- ..sim_var
    ..sim_obj$internals$start_time <- ..start_time
    ..sim_obj$config$parallel <- "none" # !!!!! Revisit this
    saveRDS(..sim_obj, file=..path_sim_obj)

  } else if (Sys.getenv("run") %in% c("main","last")) {

    tryCatch(
      ..sim_obj <- readRDS(..path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object was not found. Make sure your 'first' function",
          "is not producing errors and returns a valid simulation object, and",
          "that your shell commands are properly sequenced."))
      }
    )

    if (!class(..sim_obj)=="simba") {
      stop("Invalid simulation object")
    }

  }

  # MAIN: run simulation replicate and save results/errors
  if (Sys.getenv("run")=="main") {

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

      add_to_tid <- as.numeric(Sys.getenv("add_to_tid"))
      if (!is.na(add_to_tid)) {
        tid <- tid + add_to_tid
      }

      if (tid<1 || tid>..sim_obj$internals$num_sim_total) {
        stop(paste(
          "Task ID is invalid; must be an integer between 1 and",
          ..sim_obj$internals$num_sim_total
        ))
      } else {
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
      fmt <- paste0("%0", nchar(..sim_obj$internals$num_sim_total), "d")

      if (..sim_obj$internals$run_state=="run, no errors") {
        saveRDS(
          ..sim_obj$results,
          paste0(..path_sim_res, "/r_",
                 sprintf(fmt, ..sim_obj$internals$tid), ".rds")
        )
      } else if (..sim_obj$internals$run_state=="run, all errors") {
        saveRDS(
          ..sim_obj$errors,
          paste0(..path_sim_res, "/e_",
                 sprintf(fmt, ..sim_obj$internals$tid), ".rds")
        )
      }
    }
  }

  # LAST: merge results/errors into simulation object, run 'last' code, and save
  if (Sys.getenv("run")=="last") {

    # if there are error files in the results directory and stop_at_error is TRUE
    # skip this rep
    err_reps <- list.files(path = ..path_sim_res, pattern = "e_*")
    if (length(err_reps) > 0 & ..sim_obj$config$stop_at_error){
      ..f <- file(..path_sim_out, open="wt")
      sink(..f, type="output", append=FALSE)
      sink(..f, type="message", append=FALSE)
      cat(paste("simba output START:",Sys.time(),"\n\n"))
      cat(paste("\nSimluation stopped because of error. See error files in simba_results directory.\n\n"))
      cat(paste("\n\nsimba output END:",Sys.time(),"\n"))
      sink(type="output")
      sink(type="message")
      close(..f)

      unlink(paste0(..path_sim_res, "/r_*"))
    } else {
      # Process result/error files
      files <- dir(paste0(..path_sim_res))
      results_df <- NULL
      errors_df <- NULL
      for (file in files) {

        if (substr(file,1,1)=="r") {

          r <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(r)=="data.frame") {
            if (is.null(results_df)) {
              results_df <- r
            } else {
              results_df[nrow(results_df)+1,] <- r
            }
          }

          # !!!!! Handle non-flat result data

        } else if (substr(file,1,1)=="e") {

          e <- readRDS(paste0(..path_sim_res, "/", file))

          if (class(e)=="data.frame") {
            if (is.null(errors_df)) {
              errors_df <- e
            } else {
              errors_df[nrow(errors_df)+1,] <- e
            }
          }

        }

      }

      # Add results/errors to simulation object
      # Note: this code is somewhat redundant with the end of simba::run()
      if (!is.null(results_df) && !is.null(errors_df)) {
        ..sim_obj$results <- results_df
        ..sim_obj$errors <- errors_df
        ..sim_obj$internals$run_state <- "run, some errors"
      } else if (!is.null(results_df)) {
        ..sim_obj$results <- results_df
        ..sim_obj$errors <- "No errors"
        ..sim_obj$internals$run_state <- "run, no errors"
      } else if (!is.null(errors_df)) {
        ..sim_obj$results <- "Errors detected in 100% of simulation replicates"
        ..sim_obj$errors <- errors_df
        ..sim_obj$internals$run_state <- "run, all errors"
      } else {
        stop("An unknown error occurred.")
      }

      # record levels and num_sim that were run
      ..sim_obj$internals$levels_prev <- ..sim_obj$internals$levels_shallow
      ..sim_obj$internals$num_sim_prev <- ..sim_obj$config$num_sim
      ..sim_obj$internals$num_sim_cumulative <- ..sim_obj$internals$num_sim_cumulative + ..sim_obj$internals$num_sim_total
      ..sim_obj$internals$levels_grid_big <- create_levels_grid_big(..sim_obj)

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
      ..sim_obj$internals$end_time <- Sys.time()
      ..sim_obj$internals$total_runtime <- as.numeric(
        difftime(..sim_obj$internals$end_time, ..sim_obj$internals$start_time),
        units = "secs"
      )
      saveRDS(..sim_obj, file=..path_sim_obj)
    }

  }

}
