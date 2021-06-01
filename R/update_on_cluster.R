#' Framework for updating simulations on a cluster computing system
#'
#' @description This function provides a scaffold for updating a previously run
#'     simulation in a cluster computing environment. Like \link{run_on_cluster},
#'     it acts as a wrapper for \code{simba} code, organizing the code into sections that
#'     are run just once per simulation (e.g. changing simulation levels/replicate numbers and compiling results)
#'     and sections that are run many times (e.g. simulation replicates).
#'     This function interfaces with the cluster job scheduler to divide parallel tasks over cluster nodes.
#'     Job schedulers currently supported include Slurm and Sun Grid Engine.
#' @param first Code to run before executing additional simulation replicates. For example,
#'     this could include altering the simulation levels or changing \code{nsim}. This block of code,
#'     enclosed by curly braces {}, must first read in an existing simulation object
#'     and then make alterations to it. Global variables declared in this block will not be available when the 'main'
#'     and 'last' code blocks run.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {} that includes a call to
#'     \link[simba]{update}. This code block will have access to the simulation object you
#'     read in the 'first' code block, but any changes made here to the
#'     simulation object will not be saved.
#' @param last Code that will run after all additional simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that takes
#'     your simulation object (which at this point will contain both your old and new results)
#'     and do something with it, such as display your results on a graph.
#' @param cluster_config A list of configuration options. You must specify
#'     either \code{js} (the job scheduler you are using) or \code{tid_var} (the name of the
#'     environment variable that your task ID) is stored in. You can optionally
#'     specify \code{dir}, which is a path to a directory that will hold your
#'     simulation object and results (this defaults to the current working
#'     directory).
#' @param keep_errors logical (\code{TRUE} by default); if \code{TRUE}, do not try to re-run
#'     simulation reps that results in errors previously; if \code{FALSE}, attempt to
#'     run those reps again
#' @param keep_extra logical (\code{FALSE} by default); if \code{TRUE}, keep previously run
#'     simulation reps even if they exceed the current \code{num_sim} in config or are from
#'     a level that has been dropped; if \code{FALSE}, drop excess reps (starting from the last rep
#'     for that particular simulation level)
#' @examples
#' # The following is a toy simulation that could be run in a cluster computing environment
#' # using the SGE job scheduler. It runs 10 replicates of 2 simulation levels as 20
#' # separate cluster jobs. It then adds an additional simulation level and updates the simulation.
#' # Finally, it summarizes the results.
#'
#' # This code is saved in a file called my_simulation.R
#' library(simba)
#' run_on_cluster(
#'
#'   first = {
#'     sim %<>% new_sim()
#'     sim %<>% add_creator("create_data", function(n){ rnorm(n) })
#'     sim %<>% set_script(function() {
#'       data <- create_data(L$n)
#'       return(mean(data))
#'     })
#'     sim %<>% set_levels(n=c(100,1000))
#'     sim %<>% set_config(num_sim=10)
#'   },
#'
#'   main = {
#'     sim %<>% run()
#'   },
#'
#'   last = {},
#'
#'   cluster_config = list(js="sge")
#'
#' )
#'
#' # This code is saved in a file called run_sim.sh
#' #!/bin/bash
#' Rscript my_simulation.R
#'
#' # The following lines of code are run from the cluster head node.
#' qsub -v run='first' run_sim.sh
#' qsub -v run='main' -t 1-20 -hold_jid 101 run_sim.sh
#' qsub -v run='last' -hold_jid 102 run_sim.sh
#'
#' # This code is saved in a file called update_my_simulation.R.
#' # Note that it reads in 'sim.simba' from the previous simulation run.
#' library(simba)
#' update_on_cluster(
#'
#'   first = {
#'     sim <- readRDS('sim.simba')
#'
#'     sim %<>% set_levels(n = c(100,500,1000))
#'
#'   },
#'
#'   main = {
#'     sim %<>% update()
#'   },
#'
#'   last = {
#'     sim %<>% summarize()
#'   },
#'
#'   cluster_config = list(js = "sge")
#'
#' )
#'
#' # This code is saved in a file called update_sim.sh
#' #!/bin/bash
#' Rscript update_my_simulation.R
#'
#' # The following lines of code are run from the cluster head node.
#' # Note that only 10 new tasks are run, since 20 of 30 simulation reps
#' # were completed in the original run.
#' qsub -v run='first' update_sim.sh
#' qsub -v run='main' -t 1-10 -hold_jid 104 update_sim.sh
#' qsub -v run='last' -hold_jid 105 update_sim.sh
#'
#' @export
update_on_cluster <- function(first,
                              main,
                              last,
                              cluster_config,
                              keep_errors = TRUE,
                              keep_extra = FALSE) {

  cluster_execute(substitute(first),
                  substitute(main),
                  substitute(last),
                  cluster_config,
                  keep_errors = keep_errors,
                  keep_extra = keep_extra,
                  update_switch = TRUE)

}
