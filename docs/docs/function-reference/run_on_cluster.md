---
layout: page
title: run_on_cluster 
nav_order: 6 
permalink: /function-reference/run_on_cluster/
parent: Function reference
---


<table width="100%" summary="page for run_on_cluster {simba}"><tr><td>run_on_cluster {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Framework for running simulations on a cluster computing system</h2>

<h3>Description</h3>

<p>!!!!! TO DO. Job schedulers currently supported include Slurm, SGE, ... !!!!!
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
either js (the job scheduler you are using) or tid_var (the name of the
environment variable that your task ID) is stored in. You can optionally
specify dir, which is a path to a directory that will hold your
simulation object and results (this defaults to the current working
directory).</p>
</td></tr>
</table>


<h3>Examples</h3>

```R
!!!!! TO DO
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
