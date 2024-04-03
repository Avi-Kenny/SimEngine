#' Framework for updating simulations on a cluster computing system
#'
#' @description This function allows for simulations to be updated in parallel
#'     on a cluster computing system (CCS). See the \href{https://avi-kenny.github.io/SimEngine/articles/parallelization.html}{Parallelization}
#'     vignette for a detailed overview of how CCS parallelization works in
#'     \pkg{SimEngine}. Like \code{\link{run_on_cluster}}, the
#'     \code{update_sim_on_cluster} function acts as a wrapper for the code in
#'     your simulation, organizing the code into three sections, labeled "first"
#'     (code that is run once at the start of the simulation), "main" (running
#'     the simulation script repeatedly), and "last" (code to process or
#'     summarize simulation results). This function is to be used in conjunction
#'     with job scheduler software (e.g., Slurm or Oracle Grid Engine) to divide
#'     the simulation into tasks that are run in parallel on the CCS.
#' @param first Code to run at the start of a simulation. This should be a block
#'     of code enclosed by curly braces {} that reads in a previously-run
#'     simulation object via \code{readRDS} and makes changes to it via
#'     \code{\link{set_levels}} or \code{\link{set_config}}.
#' @param main Code that will run for every simulation replicate. This should be
#'     a block of code enclosed by curly braces {}, and will typically be a
#'     single line of code calling the \code{\link{update_sim}}) function. This
#'     code block will have access to the simulation object you created in the
#'     'first' code block, but any changes made here to the simulation object
#'     will not be saved.
#' @param last Code that will run after all simulation replicates have been run.
#'     This should be a block of code enclosed by curly braces {} that processes
#'     your simulation object (which at this point will contain your updated
#'     results), which may involve calls to \code{\link{summarize}}, creation of
#'     plots, and so on.
#' @param cluster_config A list of configuration options. You must specify
#'     either \code{js} (the job scheduler you are using) or \code{tid_var} (the
#'     name of the environment variable that your task ID is stored in); see
#'     examples. Run \code{js_support()} to see a list of job schedulers that
#'     are currently supported. You can optionally also specify \code{dir},
#'     which is a character string representing a path to a directory on the
#'     CCS; this directory will serve as your working directory and hold your
#'     simulation object and all temporary objects created by \pkg{SimEngine}.
#'     If unspecified, this defaults to the working directory of the R script
#'     that contains your simulation code).
#' @param keep_errors logical (\code{TRUE} by default); if \code{TRUE}, do not
#'     try to re-run simulation reps that results in errors previously; if
#'     \code{FALSE}, attempt to run those reps again
#' @examples
#' \dontrun{
#' # The following code is saved in a file called my_simulation.R:
#' library(SimEngine)
#' update_sim_on_cluster(
#'   first = {
#'     sim <- readRDS("sim.rds")
#'     sim %<>% set_levels(n=c(100,500,1000))
#'   },
#'   main = {
#'     sim %<>% update_sim()
#'   },
#'   last = {
#'     sim %>% summarize()
#'   },
#'   cluster_config = list(js="slurm")
#' )
#'
#' # The following code is saved in a file called run_sim.sh:
#' # #!/bin/bash
#' # Rscript my_simulation.R
#'
#' # The following lines of code are run on the CCS head node:
#' # sbatch --export=sim_run='first' run_sim.sh
#' # sbatch --export=sim_run='main' --array=1-20 --depend=afterok:101 run_sim.sh
#' # sbatch --export=sim_run='last' --depend=afterok:102 run_sim.sh
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
