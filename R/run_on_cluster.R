#' Framework for running simulations on a cluster computing system
#'
#' @param first Code to run at the start of a simulation. This should be a block
#'     of code enclosed by curly braces {} that that creates a simulation
#'     object. Put everything you need in the simulation object, since global
#'     variables declared in this block will not be available when the 'main'
#'     and 'last' code blocks run.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {} that includes a call to
#'     run(). This code block will have access to the simulation object you
#'     created in the 'first' code block, but any changes made here to the
#'     simulation object will not be saved.
#' @param last Code that will run after all simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that takes
#'     your simulation object (which at this point will contain your results)
#'     and do something with it, such as display your results on a graph.
#' @param cluster_config A list of configuration options. You must specify
#'     sim_var, which is the name of the variable you use for your simulation
#'     object. You also must specify either js (the job scheduler you are using)
#'     or tid_var (the name of the environment variable that your task ID)
#'     is stored in. You can optionally specify dir, which is a path to a
#'     directory that will hold your simulation object and results (this
#'     defaults to the current working directory).
#' @examples
#' !!!!! TO DO
#' @export
run_on_cluster <- function(first, main, last, cluster_config) {

  cfg <- cluster_config

  # !!!!! Need option to test/run code in all three sections locally; maybe with cfg$local=TRUE
  # !!!!! Need to account for situations where a simulation is run twice
  # !!!!! Need error handling for if any of the three stages fails
  # !!!!! Standardize terminology: JS or HPC ?????
  # !!!!! Need to handle loading of libraries
  # !!!!! Set start_time, end_time, etc. (check run() for others)
  # !!!!! Run everything in a separate environment
  # !!!!! Make `sim` the default sim_var

  # Check that cfg$dir is a valid direcyory
  # !!!!! Also check that it is writable by saving and deleting a test file
  if (!is.null(cfg$dir) && !dir.exists(cfg$dir)) {
    stop(paste("Directory", cfg$dir, "does not exist."))
  }

  # Construct necessary paths
  if (is.null(cfg$dir)) {
    path_sim_obj <- "sim.simba"
    path_sim_res <- "simba_results"
  } else {
    path_sim_obj <- paste0(cfg$dir, "/sim.simba")
    path_sim_res <- paste0(cfg$dir, "/simba_results")
  }

  # Error handling: incorrect Sys.getenv("run") variable
  if (!(Sys.getenv("run") %in% c("first", "last", ""))) {
    stop("The `run` environment variable must equal either 'first' or 'last'.")
  }

  # FIRST: Run 'first' code or return existing simulation object
  if (Sys.getenv("run")=="first") {

    ..start_time <- Sys.time()

    # Run 'first' code
    # !!!!! Error handling: Wrap this in a tryCatch that sets a flag if it returns and error that instructs 'main' and 'last' code to not run
    eval(substitute(first))

    # Save simulation object
    # We assume the user doesn't name their simulation object '..sim_obj'
    ..sim_obj <- eval(as.name(cfg$sim_var))
    ..sim_obj$internals$sim_var <- cfg$sim_var
    ..sim_obj$internals$start_time <- ..start_time
    saveRDS(..sim_obj, file=path_sim_obj)

    # Create directory to store simulation results
    dir.create(path_sim_res)

  } else {

    tryCatch(
      ..sim_obj <- readRDS(path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object was not found. Make sure your 'first' function",
          "is not producing errors and returns a valid simulation object, and",
          "that there are no errors in your shell scripts."))
      }
    )

  }

  # MAIN: run simulation replicate and save results/errors
  if (Sys.getenv("run")=="") {

    # Assign tid variable
    if (!is.null(cfg$tid_var)) {
      tid_var <- cfg$tid_var
    } else if (!is.null(cfg$js)) {
      if (cfg$js=="slurm") {
        tid_var <- "SLURM_ARRAY_TASK_ID"
      } else if (cfg$js=="sge") {
        tid_var <- "SGE_TASK_ID"
      } else {
        stop(paste(
          "cluster_config variable js must equal one of the following:",
          "'slurm', 'sge'."))
        # !!!!! Add more js options other than sge and slurm
      }
    } else {
      stop("You must specify either 'js' or 'tid_var' in cluster_config") # !!!!! Move some error messages to top
    }

    tid <- as.numeric(Sys.getenv(tid_var))

    if (is.na(tid)) {
      stop("Task ID is missing")
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
      eval(substitute(main))
      assign("..sim_obj", eval(as.name(..sim_obj$internals$sim_var)))
    }

    # Parse results filename and save
    fmt <- paste0("%0", nchar(..sim_obj$internals$num_sim_total), "d")

    if (..sim_obj$internals$run_state=="run, no errors") {
      saveRDS(
        ..sim_obj$results,
        paste0(path_sim_res, "/r_",
               sprintf(fmt, ..sim_obj$internals$tid), ".rds")
      )
    } else if (..sim_obj$internals$run_state=="run, all errors") {
      saveRDS(
        ..sim_obj$errors,
        paste0(path_sim_res, "/e_",
               sprintf(fmt, ..sim_obj$internals$tid), ".rds")
      )
    }

  }

  # LAST: merge results/errors into simulation object, run 'last' code, and save
  if (Sys.getenv("run")=="last") {

    # Process result/error files
    files <- dir(paste0(path_sim_res))
    results_df <- NULL
    errors_df <- NULL
    for (file in files) {

      if (substr(file,1,1)=="r") {

        r <- readRDS(paste0(path_sim_res, "/", file))

        if (class(r)=="data.frame") {
          if (is.null(results_df)) {
            results_df <- r
          } else {
            results_df[nrow(results_df)+1,] <- r
          }
        }

      } else if (substr(file,1,1)=="e") {

        e <- readRDS(paste0(path_sim_res, "/", file))

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
      ..sim_obj$errors <- "No errors."
      ..sim_obj$internals$run_state <- "run, no errors"
    } else if (!is.null(errors_df)) {
      ..sim_obj$results <- "Errors detected in 100% of simulation replicates."
      ..sim_obj$errors <- errors_df
      ..sim_obj$internals$run_state <- "run, all errors"
    } else {
      stop("An unknown error occurred.")
    }

    # Run 'last' code
    for (pkg in ..sim_obj$config$packages) {
      do.call("library", list(pkg))
    }
    assign(..sim_obj$internals$sim_var, ..sim_obj)
    eval(substitute(last))
    assign("..sim_obj", eval(as.name(..sim_obj$internals$sim_var)))

    # Save final simulation object and delete intermediate files
    ..sim_obj$internals$end_time <- Sys.time()
    ..sim_obj$internals$total_runtime <- as.numeric(
      difftime(..sim_obj$internals$end_time, ..sim_obj$internals$start_time),
      units = "secs"
    )
    saveRDS(..sim_obj, file=path_sim_obj)
    unlink(path_sim_res, recursive=TRUE)

  }

}
