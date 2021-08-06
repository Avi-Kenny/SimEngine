---
layout: page
title: run_on_cluster 
nav_order: 8 
permalink: /function-reference/run_on_cluster/
parent: Function reference
---


<table width="100%" summary="page for run_on_cluster {simba}"><tr><td>run_on_cluster {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Framework for running simulations on a cluster computing system</h2>

<h3>Description</h3>

<p>This function provides a scaffold for running simulations in
parallel in a cluster computing environment. It acts as a wrapper for
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span> simulation code, organizing the code into sections that are
run just once per simulation (e.g. simulation setup and compiling
results) and sections that are run many times (e.g. simulation
replicates). This function interfaces with the cluster job scheduler to
divide parallel tasks over cluster nodes. Job schedulers currently
supported include Slurm and Sun Grid Engine.
</p>


<h3>Usage</h3>

```R
run_on_cluster(first, main, last, cluster_config)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>first</span></td>
<td>
<p>Code to run at the start of a simulation. This should be a block
of code enclosed by curly braces  that that creates a simulation
object. Put everything you need in the simulation object, since global
variables declared in this block will not be available when the 'main'
and 'last' code blocks run.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>main</span></td>
<td>
<p>Code that will run for every simulation replicate. This should be
a block of code enclosed by curly braces  that includes a call to
run. This code block will have access to the simulation object you
created in the 'first' code block, but any changes made here to the
simulation object will not be saved.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>last</span></td>
<td>
<p>Code that will run after all simulation replicates have been run.
This should be a block of code enclosed by curly braces  that takes
your simulation object (which at this point will contain your results)
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
</table>


<h3>Examples</h3>

```R
## Not run: 
# The following is a toy simulation that could be run in a cluster computing
# environment using the Oracle Grid Engine job scheduler. It runs 10
# replicates of 2 simulation levels as 20 separate cluster jobs, and then
# summarizes the results. If a different scheduler is being used, modify the
# shell script accordingly (i.e. change the qsub commands to the relevant
# commands for your scheduler.

# This code is saved in a file called my_simulation.R
library(simba)
run_on_cluster(

  first = {
    sim <- new_sim()
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

  last = {
    sim %<>% summarize()
  },

  cluster_config = list(js="ge")

)

# This code is saved in a file called run_sim.sh
#!/bin/bash
Rscript my_simulation.R

# The following lines of code are run from the cluster head node.
qsub -v run='first' run_sim.sh
qsub -v run='main' -t 1-20 -hold_jid 101 run_sim.sh
qsub -v run='last' -hold_jid 102 run_sim.sh

## End(Not run)
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
