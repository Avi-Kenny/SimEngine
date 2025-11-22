# Framework for updating simulations on a cluster computing system

This function allows for simulations to be updated in parallel on a
cluster computing system (CCS). See the
[Parallelization](https://avi-kenny.github.io/SimEngine/articles/parallelization.html)
vignette for a detailed overview of how CCS parallelization works in
SimEngine. Like
[`run_on_cluster`](https://avi-kenny.github.io/SimEngine/reference/run_on_cluster.md),
the `update_sim_on_cluster` function acts as a wrapper for the code in
your simulation, organizing the code into three sections, labeled
"first" (code that is run once at the start of the simulation), "main"
(running the simulation script repeatedly), and "last" (code to process
or summarize simulation results). This function is to be used in
conjunction with job scheduler software (e.g., Slurm or Oracle Grid
Engine) to divide the simulation into tasks that are run in parallel on
the CCS.

## Usage

``` r
update_sim_on_cluster(first, main, last, cluster_config, keep_errors = T)
```

## Arguments

- first:

  Code to run at the start of a simulation. This should be a block of
  code enclosed by curly braces that reads in a previously-run
  simulation object via `readRDS` and makes changes to it via
  [`set_levels`](https://avi-kenny.github.io/SimEngine/reference/set_levels.md)
  or
  [`set_config`](https://avi-kenny.github.io/SimEngine/reference/set_config.md).

- main:

  Code that will run for every simulation replicate. This should be a
  block of code enclosed by curly braces , and will typically be a
  single line of code calling the
  [`update_sim`](https://avi-kenny.github.io/SimEngine/reference/update_sim.md))
  function. This code block will have access to the simulation object
  you created in the 'first' code block, but any changes made here to
  the simulation object will not be saved.

- last:

  Code that will run after all simulation replicates have been run. This
  should be a block of code enclosed by curly braces that processes your
  simulation object (which at this point will contain your updated
  results), which may involve calls to
  [`summarize`](https://avi-kenny.github.io/SimEngine/reference/summarize.md),
  creation of plots, and so on.

- cluster_config:

  A list of configuration options. You must specify either `js` (the job
  scheduler you are using) or `tid_var` (the name of the environment
  variable that your task ID is stored in); see examples. Run
  [`js_support()`](https://avi-kenny.github.io/SimEngine/reference/js_support.md)
  to see a list of job schedulers that are currently supported. You can
  optionally also specify `dir`, which is a character string
  representing a path to a directory on the CCS; this directory will
  serve as your working directory and hold your simulation object and
  all temporary objects created by SimEngine. If unspecified, this
  defaults to the working directory of the R script that contains your
  simulation code).

- keep_errors:

  logical (`TRUE` by default); if `TRUE`, do not try to re-run
  simulation reps that results in errors previously; if `FALSE`, attempt
  to run those reps again

## Examples

``` r
if (FALSE) { # \dontrun{
# The following code is saved in a file called my_simulation.R:
library(SimEngine)
update_sim_on_cluster(
  first = {
    sim <- readRDS("sim.rds")
    sim %<>% set_levels(n=c(100,500,1000))
  },
  main = {
    sim %<>% update_sim()
  },
  last = {
    sim %>% summarize()
  },
  cluster_config = list(js="slurm")
)

# The following code is saved in a file called run_sim.sh:
# #!/bin/bash
# Rscript my_simulation.R

# The following lines of code are run on the CCS head node:
# sbatch --export=sim_run='first' run_sim.sh
# sbatch --export=sim_run='main' --array=1-20 --depend=afterok:101 run_sim.sh
# sbatch --export=sim_run='last' --depend=afterok:102 run_sim.sh

# Note: some users may need to use --export=ALL,sim_run='first' and so on for
#   the above three commands for code to run properly.
} # }
```
