#' Framework function for running simulations on a cluster computing system
#'
#' @param first Code to run at the start of a simulation. This should be a
#'     function that creates and returns a simulation object. Put everything you
#'     need in the simulation object.
#' @param main Code that will run for every simulation replicate. This should
#'     be a function that takes in your simulation object and returns the
#'     results of a call to run()
#' @param last Code that will run after all simulation replicates have been run.
#'     This should be a function that takes in your simulation object (which at
#'     this point will contain your results) and does something with it (e.g.
#'     displays the results on a graph).
#' @param config A list of configuration options. !!!!! TO DO
#' @examples
#' !!!!! TO DO
#' @export
run_on_cluster <- function(first, main, last, config) {

  # Check that config$dir is a valid direcyory
  if (!is.null(config$dir) & !dir.exists(config$dir)) {
    stop(paste("Directory", config$dir, "does not exist."))
  }

  # Construct necessary paths
  if (is.null(config$dir)) {
    path_sim_obj <- "sim.simba"
    path_sim_res <- "simba_results"
  } else {
    path_sim_obj <- paste0(config$dir, "/sim.simba")
    path_sim_res <- paste0(config$dir, "/simba_results")
  }

  # Run 'first' function or return existing simulation object
  if (Sys.getenv("run")=="first") {

    sim_obj <- first()
    saveRDS(sim_obj, path_sim_obj)
    dir.create(path_sim_res)

  } else {

    tryCatch(
      sim_obj <- readRDS(path_sim_obj),
      warning = function(w) {
        stop(paste(
          "Simulation object not found. Make sure your 'first' function works",
          "and returns a valid simulation object, and that you are sequencing",
          "your scripts appropriately.")) # !!!!!
      }
    )

  }

  # Run 'main' function
  if (Sys.getenv("run")=="") {

    # Assign tid variable
    if (!is.null(config$js)) {
      if (config$js=="slurm") {
        tid_name <- "SLURM_ARRAY_TASK_ID"
      } else if (config$js=="sge") {
        tid_name <- "SGE_TASK_ID"
      } else {
        # !!!!! Consider ditching config$js
        stop("config$js must equal one of the following: 'slurm', 'sge'.")
      }
    }
    if (!is.null(config$tid_name)) {
      tid_name <- config$tid_name
    }
    if (Sys.getenv(tid_name)=="") {
      stop("Task ID is missing")
    }
    if (FALSE) {
      # !!!!! Check if tid is in valid range
      stop(paste(
        "Task ID is invalid; must be an integer between 1 and",
        sim_obj$internals$num_sim_total
      ))
    }

    # Set tid and run 'main' code
    sim_obj$internals$tid <- as.numeric(Sys.getenv(tid_name))
    sim_obj <- main(sim_obj)

    # Parse results filename and save
    # !!!!! Check sim_obj for results/errors and save files accordingly
    fmt <- paste0("%0", nchar(sim_obj$internals$num_sim_total), "d")
    saveRDS(
      sim_obj$results,
      paste0(path_sim_res, "/s_", sprintf(fmt, sim_obj$internals$tid), ".rds")
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
  }

  # Error handling: incorrect Sys.getenv("run") variable
  if (!(Sys.getenv("run") %in% c("first", "last", ""))) {
    stop("The `run` environment variable must equal either 'first' or 'last'.")
  }

}



# !!!!! TESTING !!!!!

# First
Sys.setenv(run="first")

# Main
Sys.setenv(run="")
Sys.setenv(SLURM_ARRAY_TASK_ID="1")
# sim <- readRDS("testfolder1/sim.simba")
# res <- readRDS("testfolder1/simba_results/s_01.rds")

# Last
Sys.setenv(run="last")

run_on_cluster(

  first = function() {

    sim <- new_sim()
    sim %<>% set_config(parallel = "cluster", num_sim=12)
    sim %<>% add_creator("my_creator",function(x){x^2})
    sim %<>% add_script("my_script", function(){
      return(list(x=rnorm(1), y=rnorm(1)))
    })
    return (sim)

  },

  main = function(sim) {

    sim %<>% run("my_script")
    return (sim)

  },

  last = function(sim) {

    sim %>% summary() %>% print()

  },

  config = list(js="slurm", dir="testfolder1")

)
