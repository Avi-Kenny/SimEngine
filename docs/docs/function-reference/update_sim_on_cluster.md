---
layout: page
title: update_sim_on_cluster 
nav_order: 12 
permalink: /function-reference/update_sim_on_cluster/
parent: Function reference
---

<script type="text/javascript">
const macros = { "\\R": "\\textsf{R}", "\\code": "\\texttt"};
function processMathHTML() {
    var l = document.getElementsByClassName('reqn');
    for (let e of l) { katex.render(e.textContent, e, { throwOnError: false, macros }); }
    return;
}</script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.js"
    onload="processMathHTML();"></script>
<link rel="stylesheet" type="text/css" href="R.css" />
</head><body><div class="container">

<table style="width: 100%;"><tr><td>update_sim_on_cluster {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Framework for updating simulations on a cluster computing system</h2>

<h3>Description</h3>

<p>This function serves a scaffold for updating a previously-run in
parallel on a cluster computing system. Like
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>run_on_cluster</span>, it acts as a wrapper for the code in your
simulation script, organizing the code into three sections, labeled
&quot;first&quot; (code that is run once at the start of the simulation, e.g.
setting simulation levels), &quot;main&quot; (the simulation script, which is run
repeatedly), and &quot;last&quot; (code to combine and summarize simulation
results). This function interacts with cluster job scheduler software
(e.g. Slurm or Oracle Grid Engine) to divide parallel tasks over cluster
nodes. See <a href="https://avi-kenny.github.io/SimEngine/parallelization/">https://avi-kenny.github.io/SimEngine/parallelization/</a>
for an overview of how cluster parallelization works in <span class="pkg">SimEngine</span>.
</p>


<h3>Usage</h3>

```R<code class='language-R'>update_sim_on_cluster(first, main, last, cluster_config, keep_errors = T)
</span>```


<h3>Arguments</h3>

<table>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>first</span></td>
<td>
<p>Code to run before executing additional simulation replicates. For example,
this could include altering the simulation levels or changing <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>nsim</span>. This block of code,
enclosed by curly braces , must first read in an existing simulation object
and then make alterations to it. Global variables declared in this block will not be available when the 'main'
and 'last' code blocks run.</p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>main</span></td>
<td>
<p>Code that will run for every simulation replicate. This should be
a block of code enclosed by curly braces  that includes a call to
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>update_sim</span>. This code block will have access to the
simulation object you read in the 'first' code block, but any changes
made here to the simulation object will not be saved.</p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>last</span></td>
<td>
<p>Code that will run after all additional simulation replicates have been run.
This should be a block of code enclosed by curly braces  that takes
your simulation object (which at this point will contain both your old and new results)
and do something with it, such as display your results on a graph.</p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>cluster_config</span></td>
<td>
<p>A list of configuration options. You must specify
either <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>js</span> (the job scheduler you are using) or <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>tid_var</span> (the
name of the environment variable that your task ID is stored in). Run
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>js_support()</span> to see a list of job schedulers that are currently
supported. You can optionally also specify <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>dir</span>, which is a
character string representing a path to a directory; this directory will
serve as your working directory and hold your simulation object,
temporary <span class="pkg">SimEngine</span> objects, and simulation results (this defaults
to the working directory of the R script that contains your simulation
code).</p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_errors</span></td>
<td>
<p>logical (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span> by default); if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span>, do not
try to re-run simulation reps that results in errors previously; if
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>, attempt to run those reps again</p>
</td></tr>
</table>


<h3>Examples</h3>

```R<code class='language-R'>## Not run: 
# The following code creates, runs, and subsequently updates a toy simulation
# on a cluster computing environment. We include both the R code as well as
# sample BASH code for running the simulation using Oracle Grid Engine.

# This code is saved in a file called my_simulation.R
library(SimEngine)
run_on_cluster(

  first = {
    sim <- new_sim()
    create_data <- function(n) { rnorm(n) }
    sim %<>% set_script(function() {
      data <- create_data(L$n)
      return(list("x"=mean(data)))
    })
    sim %<>% set_levels(n=c(100,1000))
    sim %<>% set_config(num_sim=10)
  },

  main = {
    sim %<>% run()
  },

  last = {
    sim %>% summarize()
  },

  cluster_config = list(js="ge")

)

# This code is saved in a file called run_sim.sh
# #!/bin/bash
# Rscript my_simulation.R

# The following lines of code are run on the cluster head node.
# qsub -v sim_run='first' run_sim.sh
# qsub -v sim_run='main' -t 1-20 -hold_jid 101 run_sim.sh
# qsub -v sim_run='last' -hold_jid 102 run_sim.sh

# This code is saved in a file called update_my_simulation.R. Note that it
# reads in the simulation object created above, which is saved in a file
# called "sim.rds".
library(SimEngine)
update_sim_on_cluster(

  first = {
    sim <- readRDS("sim.rds")
    sim %<>% set_levels(n = c(100,500,1000))
  },

  main = {
    sim %<>% update_sim()
  },

  last = {
    sim %>% summarize()
  },

  cluster_config = list(js="ge")

)

# This code is saved in a file called update_sim.sh
# #!/bin/bash
# Rscript update_my_simulation.R

# The following lines of code are run on the cluster head node. Note that
# only 10 new replicates are run, since 20 of 30 simulation replicates were
# run in the original call to run_on_cluster.
# qsub -v sim_run='first' update_sim.sh
# qsub -v sim_run='main' -t 1-10 -hold_jid 104 update_sim.sh
# qsub -v sim_run='last' -hold_jid 105 update_sim.sh

## End(Not run)
</span>```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.1.0 ]</div>
</div>
