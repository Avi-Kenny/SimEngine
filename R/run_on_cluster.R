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
run_on_cluster <- function(first,
                           main,
                           last,
                           cluster_config) {

  cluster_execute(substitute(first),
                  substitute(main),
                  substitute(last),
                  cluster_config)
}
