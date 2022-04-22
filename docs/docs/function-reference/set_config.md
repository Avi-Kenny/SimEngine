---
layout: page
title: set_config 
nav_order: 6 
permalink: /function-reference/set_config/
parent: Function reference
---


<table width="100%" summary="page for set_config {SimEngine}"><tr><td>set_config {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

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
  sim,
  num_sim = 1000,
  parallel = "none",
  n_cores = parallel::detectCores() - 1,
  packages = NULL,
  stop_at_error = FALSE,
  progress_bar = TRUE,
  seed = NA
)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span></p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim</span></td>
<td>
<p>An integer; the number of simulations to conduct for each
level combination</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>parallel</span></td>
<td>
<p>A string; one of c(&quot;outer&quot;, &quot;inner&quot;, &quot;none&quot;). Controls which
sections of the code are parallelized. Setting to &quot;outer&quot; will run one
simulation per core. Setting to &quot;inner&quot; will allow for parallelization
within a single simulation replicate. Setting to &quot;none&quot; will not
parallelize any code. See
<a href="https://avi-kenny.github.io/SimEngine/parallelization/">https://avi-kenny.github.io/SimEngine/parallelization/</a> for an
overview of how parallelization works in <span class="pkg">SimEngine</span>. This option
will be ignored if the simulation is being run on a cluster computing
system.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>n_cores</span></td>
<td>
<p>An integer; determines the number of CPUs on which the simulation
will run if using parallelization. Defaults to one fewer than the number of
available CPUs on the current host.</p>
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
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>progress_bar</span></td>
<td>
<p>A Boolean. If set to FALSE, the progress bar that is
normally displayed while the simulation is running is suppressed.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>seed</span></td>
<td>
<p>An integer; seeds allow for reproducible simulation results. If a
seed is specified, then consecutive runs of the same simulation with the
same seed will lead to identical results (under normal circumstances). If
a seed was not set in advance by the user, <span class="pkg">SimEngine</span> will set a
random seed, which can later be retrieved using the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>vars</span>
function. See details for further info.</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>If a user specifies, for example, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config(seed=4)</span>, this
seed is used twice by <span class="pkg">SimEngine</span>. First, <span class="pkg">SimEngine</span> executes
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set.seed(4)</span> at the end of the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span> call. Second, this
seed is used to generate a new set of seeds, one for each simulation
replicate. Each of these seeds is set in turn (or in parallel) when
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>run</span> is called.
</p>
</li>
<li><p>Even if seeds are used, not all code will be reproducible. For
example, a simulation that involves getting the current date/time with
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>Sys.time()</span> or dynamically retrieving external data may produce
different results on different runs.
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with a modified configuration
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% set_config(
  num_sim = 10,
  seed = 2112
)
sim
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
