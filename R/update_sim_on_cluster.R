#' Framework for updating simulations on a cluster computing system
#'
#' @description This function serves a scaffold for updating a previously-run in
#'     parallel on a cluster computing system. Like
#'     \code{\link{run_on_cluster}}, it acts as a wrapper for the code in your
#'     simulation script, organizing the code into three sections, labeled
#'     "first" (code that is run once at the start of the simulation, e.g.
#'     setting simulation levels), "main" (the simulation script, which is run
#'     repeatedly), and "last" (code to combine and summarize simulation
#'     results). This function interacts with cluster job scheduler software
#'     (e.g. Slurm or Oracle Grid Engine) to divide parallel tasks over cluster
#'     nodes. See \url{https://avi-kenny.github.io/SimEngine/parallelization/}
#'     for an overview of how cluster parallelization works in \pkg{SimEngine}.
#' @param first Code to run before executing additional simulation replicates. For example,
#'     this could include altering the simulation levels or changing \code{nsim}. This block of code,
#'     enclosed by curly braces {}, must first read in an existing simulation object
#'     and then make alterations to it. Global variables declared in this block will not be available when the 'main'
#'     and 'last' code blocks run.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {} that includes a call to
#'     \code{\link{update_sim}}. This code block will have access to the
#'     simulation object you read in the 'first' code block, but any changes
#'     made here to the simulation object will not be saved.
#' @param last Code that will run after all additional simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that takes
#'     your simulation object (which at this point will contain both your old and new results)
#'     and do something with it, such as display your results on a graph.
#' @param cluster_config A list of configuration options. You must specify
#'     either \code{js} (the job scheduler you are using) or \code{tid_var} (the
#'     name of the environment variable that your task ID is stored in). Run
#'     \code{js_support()} to see a list of job schedulers that are currently
#'     supported. You can optionally also specify \code{dir}, which is a
#'     character string representing a path to a directory; this directory will
#'     serve as your working directory and hold your simulation object,
#'     temporary \pkg{SimEngine} objects, and simulation results (this defaults
#'     to the working directory of the R script that contains your simulation
#'     code).
#' @param keep_errors logical (\code{TRUE} by default); if \code{TRUE}, do not
#'     try to re-run simulation reps that results in errors previously; if
#'     \code{FALSE}, attempt to run those reps again
#' @examples
#' \dontrun{
#' # The following code creates, runs, and subsequently updates a toy simulation
#' # on a cluster computing environment. We include both the R code as well as
#' # sample BASH code for running the simulation using Oracle Grid Engine.
#'
#' # This code is saved in a file called my_simulation.R
#' library(SimEngine)
#' run_on_cluster(
#'
#'   first = {
#'     sim <- new_sim()
#'     create_data <- function(n) { rnorm(n) }
#'     sim %<>% set_script(function() {
#'       data <- create_data(L$n)
#'       return(list("x"=mean(data)))
#'     })
#'     sim %<>% set_levels(n=c(100,1000))
#'     sim %<>% set_config(num_sim=10)
#'   },
#'
#'   main = {
#'     sim %<>% run()
#'   },
#'
#'   last = {
#'     sim %>% summarize()
#'   },
#'
#'   cluster_config = list(js="ge")
#'
#' )
#'
#' # This code is saved in a file called run_sim.sh
#' # #!/bin/bash
#' # Rscript my_simulation.R
#'
#' # The following lines of code are run on the cluster head node.
#' # qsub -v sim_run='first' run_sim.sh
#' # qsub -v sim_run='main' -t 1-20 -hold_jid 101 run_sim.sh
#' # qsub -v sim_run='last' -hold_jid 102 run_sim.sh
#'
#' # This code is saved in a file called update_my_simulation.R. Note that it
#' # reads in the simulation object created above, which is saved in a file
#' # called "sim.rds".
#' library(SimEngine)
#' update_sim_on_cluster(
#'
#'   first = {
#'     sim <- readRDS("sim.rds")
#'     sim %<>% set_levels(n = c(100,500,1000))
#'   },
#'
#'   main = {
#'     sim %<>% update_sim()
#'   },
#'
#'   last = {
#'     sim %>% summarize()
#'   },
#'
#'   cluster_config = list(js="ge")
#'
#' )
#'
#' # This code is saved in a file called update_sim.sh
#' # #!/bin/bash
#' # Rscript update_my_simulation.R
#'
#' # The following lines of code are run on the cluster head node. Note that
#' # only 10 new replicates are run, since 20 of 30 simulation replicates were
#' # run in the original call to run_on_cluster.
#' # qsub -v sim_run='first' update_sim.sh
#' # qsub -v sim_run='main' -t 1-10 -hold_jid 104 update_sim.sh
#' # qsub -v sim_run='last' -hold_jid 105 update_sim.sh
#' }
#' @export
update_sim_on_cluster <- function(
  first, main, last, cluster_config, keep_errors=T
) {

  cluster_execute(
    substitute(first),
    substitute(main),
    substitute(last),
    cluster_config,
    keep_errors = keep_errors,
    update_switch = T
  )

}
