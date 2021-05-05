---
layout: page
title: set_config 
nav_order: 7 
permalink: /function-reference/set_config/
parent: Function reference
---


<table width="100%" summary="page for set_config {simba}"><tr><td>set_config {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Modify the simulation configuration</h2>

<h3>Description</h3>

<p>This function sets configuration options for the simulation. If
the 'packages' argument is specified, all packages will be loaded and
attached via <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>library</span> when <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span> is called. Multiple
calls to <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span> will only overwrite configuration options that
are specified in the subsequent calls, leaving others in place. You can
see the current configuration via <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>print(sim)</span>, where <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span> is
your simulation object.
</p>


<h3>Usage</h3>

```R
set_config(
  sim_obj,
  num_sim = 1000,
  datasets = "many",
  parallel = "none",
  parallel_cores = parallel::detectCores() - 1,
  packages = NULL,
  stop_at_error = FALSE,
  seed = 1,
  dir = getwd()
)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim</span></td>
<td>
<p>An integer; the number of simulations to conduct for each
level combination</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>datasets</span></td>
<td>
<p>A string; either &quot;one&quot; or &quot;many&quot;. If set to &quot;one&quot;, the same
dataset will be used for all simulations. If set to &quot;many&quot;, a new
dataset will be generated for each simulation.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>parallel</span></td>
<td>
<p>String; either &quot;outer&quot;, &quot;inner&quot;, or &quot;none&quot;. Controls which
sections of the code are parallelized. Setting to &quot;outer&quot; will run one
simulation per core. Setting to &quot;inner&quot; will allow for parallelization
within a single simulation replicate. Setting to &quot;none&quot; will not
parallelize any code. See
<a href="https://avi-kenny.github.io/simba/parallelization">https://avi-kenny.github.io/simba/parallelization</a> for an overview
of how parallelization works in <span class="pkg">simba</span>.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>packages</span></td>
<td>
<p>A character vector of packages to load and attach</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stop_at_error</span></td>
<td>
<p>A Boolean. If set to TRUE, the simulation will
stop if it encounters an error in any single replicate Useful for
debugging.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>seed</span></td>
<td>
<p>An integer; seeds allow for reproducible simulation results.
Normally, when a given simulation is run multiple times, it will give
the same results each time unless the seed is changed using
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span>.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>dir</span></td>
<td>
<p>A directory (given as a character string) where simulation files
should be stored; if this option is not set, files will be stored in the
current working directory.</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>If a user specifies, for example, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config(seed=4)</span>, this
seed is used twice by <span class="pkg">simba</span>. First, <span class="pkg">simba</span> executes
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set.seed(4)</span> at the end of the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span> call. Second,
when run is called, <span class="pkg">simba</span> will execute
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set.seed(as.integer(4*sim_uid))</span> at the start of simulation
replicate, where <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span> is the unique identifier corresponding
to that replicate. This is necessary to ensure that results are
reproducible even when simulations involve parallelization.
</p>
</li>
<li><p>Even if seeds are used, not all code will be reproducible. For
example, a simulation that involves getting the current date/time with
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>Sys.time()</span> may produce different results on different runs.
</p>
</li>
<li><p>Setting seeds is not currently with inner parallelization
(<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config(parallel="inner")</span>).
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with a modified configuration
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% set_config(
  num_sim = 10
)
sim
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
