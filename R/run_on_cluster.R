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
#'     created in the 'first' code block.
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

  # !!!!! Standardize terminology: JS or HPC ?????

  # Check that cfg$dir is a valid direcyory
  if (!is.null(cfg$dir) & !dir.exists(cfg$dir)) {
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

  # Run 'first' function or return existing simulation object
  if (Sys.getenv("run")=="first") {

    # Run 'first' code
    # !!!!! Error handling: Wrap this in a tryCatch that sets a flag if it returns and error that instructs 'main' and 'last' code to not run
    first # !!!!! Is eval(substitute()) needed (to specify environment) ?????

    # Save simulation object
    # We assume the user doesn't name their simulation object '..sim_obj'
    ..sim_obj <- eval(as.name(cfg$sim_var))
    ..sim_obj$internals$sim_var <- cfg$sim_var
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

  # Run 'main' function
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

    if (tid<1 | tid>sim_obj$internals$num_sim_total) {
      stop(paste(
        "Task ID is invalid; must be an integer between 1 and",
        ..sim_obj$internals$num_sim_total
      ))
    } else {
      ..sim_obj$internals$tid <- tid
      rm(tid)
      assign(..sim_obj$internals$sim_var, ..sim_obj)
      main
      assign("..sim_obj", eval(as.name(..sim_obj$internals$sim_var)))
    }

    # Parse results filename and save
    # !!!!! Check sim_obj for results/errors and save files accordingly
    fmt <- paste0("%0", nchar(..sim_obj$internals$num_sim_total), "d")
    saveRDS(
      ..sim_obj$results,
      paste0(path_sim_res, "/r_", sprintf(fmt, .tid), ".rds")
    )
    saveRDS(
      ..sim_obj$errors,
      paste0(path_sim_res, "/e_", sprintf(fmt, .tid), ".rds")
    )
    # !!!!! Need to account for situations where a simulation is run twice

    # !!!!! Need error handling for if any of the three stages fails

  }

  # Run 'last' function
  if (Sys.getenv("run")=="last") {

    # path_sim_res
    files <- dir(path_sim_res)
    for (i in 1:length(files)) {
      res <- readRDS(paste0(path_sim_res, "/", files[i]))
      print(res)
      # !!!!! Merge into sim_obj
    }

    # !!!!! merge results and errors into sim_obj
    last(sim_obj)

    # !!!!! Save final sim_obj and delete intermediate sim_objs

  }

  # Error handling: incorrect Sys.getenv("run") variable
  if (!(Sys.getenv("run") %in% c("first", "last", ""))) {
    stop("The `run` environment variable must equal either 'first' or 'last'.")
  }

}



testfunc <- function(code, flag) {
  if (flag) {
    # print(c)
    eval(substitute(code))
    # code
  }
  # print(z)
}

hey <-
testfunc({
  x <- 3
  y <- 4
  print(x+y)
}, TRUE)








