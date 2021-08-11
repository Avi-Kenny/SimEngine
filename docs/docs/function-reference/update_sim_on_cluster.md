---
layout: page
title: update_sim_on_cluster 
nav_order: 14 
permalink: /function-reference/update_sim_on_cluster/
parent: Function reference
---


<table width="100%" summary="page for update_sim_on_cluster {simba}"><tr><td>update_sim_on_cluster {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Framework for updating simulations on a cluster computing system</h2>

<h3>Description</h3>

<p>This function provides a scaffold for updating a previously run
simulation in a cluster computing environment. Like run_on_cluster,
it acts as a wrapper for <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span> code, organizing the code into sections that
are run just once per simulation (e.g. changing simulation levels/replicate numbers and compiling results)
and sections that are run many times (e.g. simulation replicates).
This function interfaces with the cluster job scheduler to divide parallel tasks over cluster nodes.
Job schedulers currently supported include Slurm and Sun Grid Engine.
</p>


<h3>Usage</h3>

```R
update_sim_on_cluster(
  first,
  main,
  last,
  cluster_config,
  keep_errors = TRUE,
  keep_extra = FALSE
)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>first</span></td>
<td>
<p>Code to run before executing additional simulation replicates. For example,
this could include altering the simulation levels or changing <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>nsim</span>. This block of code,
enclosed by curly braces , must first read in an existing simulation object
and then make alterations to it. Global variables declared in this block will not be available when the 'main'
and 'last' code blocks run.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>main</span></td>
<td>
<p>Code that will run for every simulation replicate. This should be
a block of code enclosed by curly braces  that includes a call to
update_sim. This code block will have access to the simulation object you
read in the 'first' code block, but any changes made here to the
simulation object will not be saved.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>last</span></td>
<td>
<p>Code that will run after all additional simulation replicates have been run.
This should be a block of code enclosed by curly braces  that takes
your simulation object (which at this point will contain both your old and new results)
and do something with it, such as display your results on a graph.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>cluster_config</span></td>
<td>
<p>A list of configuration options. You must specify
either <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>js</span> (the job scheduler you are using) or <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>tid_var</span> (the
name of the environment variable that your task ID is stored in). Run
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>js_support()</span> to see a list of job schedulers that are currently
supported. You can optionally also specify <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>dir</span>, which is a
character string representing a path to a directory; this directory will
serve as your working directory and hold your simulation object,
temporary <span class="pkg">simba</span> objects, and simulation results (this defaults to
the working directory of the R script that contains your simulation
code).</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_errors</span></td>
<td>
<p>logical (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span> by default); if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span>, do not try to re-run
simulation reps that results in errors previously; if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>, attempt to
run those reps again</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_extra</span></td>
<td>
<p>logical (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span> by default); if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span>, keep previously run
simulation reps even if they exceed the current <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim</span> in config or are from
a level that has been dropped; if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>, drop excess reps (starting from the last rep
for that particular simulation level)</p>
</td></tr>
</table>


<h3>Examples</h3>

```R
## Not run: 
# The following is a toy simulation that could be run in a cluster computing
# environment using the Oracle Grid Engine job scheduler. It runs 10
# replicates of 2 simulation levels as 20 separate cluster jobs. It then adds
# an additional simulation level and updates the simulation. Finally, it
# summarizes the results.

# This code is saved in a file called my_simulation.R
library(simba)
run_on_cluster(

  first = {
    sim %<>% new_sim()
    sim %<>% add_creator("create_data", function(n){ rnorm(n) })
    sim %<>% set_script(function() {
      data <- create_data(L$n)
      return(mean(data))
    })
    sim %<>% set_levels(n=c(100,1000))
    sim %<>% set_config(num_sim=10)
  },

  main = {
    sim %<>% run()
  },

  last = {},

  cluster_config = list(js="ge")

)

# This code is saved in a file called run_sim.sh
#!/bin/bash
Rscript my_simulation.R

# The following lines of code are run from the cluster head node.
qsub -v run='first' run_sim.sh
qsub -v run='main' -t 1-20 -hold_jid 101 run_sim.sh
qsub -v run='last' -hold_jid 102 run_sim.sh

# This code is saved in a file called update_my_simulation.R.
# Note that it reads in 'sim.simba' from the previous simulation run.
library(simba)
update_sim_on_cluster(

  first = {
    sim <- readRDS('sim.simba')

    sim %<>% set_levels(n = c(100,500,1000))

  },

  main = {
    sim %<>% update_sim()
  },

  last = {
    sim %<>% summarize()
  },

  cluster_config = list(js = "ge")

)

# This code is saved in a file called update_sim.sh
#!/bin/bash
Rscript update_my_simulation.R

# The following lines of code are run from the cluster head node.
# Note that only 10 new tasks are run, since 20 of 30 simulation reps
# were completed in the original run.
qsub -v run='first' update_sim.sh
qsub -v run='main' -t 1-10 -hold_jid 104 update_sim.sh
qsub -v run='last' -hold_jid 105 update_sim.sh

## End(Not run)
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 1.0.0 ]</div>
