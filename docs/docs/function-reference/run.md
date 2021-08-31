---
layout: page
title: run 
nav_order: 7 
permalink: /function-reference/run/
parent: Function reference
---


<table width="100%" summary="page for run {SimEngine}"><tr><td>run {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Run the simulation</h2>

<h3>Description</h3>

<p>This is the workhorse function of <span class="pkg">SimEngine</span> that actually
runs the simulation. This should be called after all functions that set
up the simulation (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>add_creator</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span>, etc.) have been
called.
</p>


<h3>Usage</h3>

```R
run(sim, sim_uids = NA)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uids</span></td>
<td>
<p>Advanced; a vector of <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span> values, each of which
uniquely identifies a simulation replicate. This will normally be
omitted. If this is specified, only the simulation replicates with a
matching <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span> will be run.</p>
</td></tr>
</table>


<h3>Value</h3>

<p>The original simulation object but with the results attached (along
with any errors and warnings). Results are stored in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>,
errors are stored in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$errors</span>, and warnings are stored in
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$warnings</span>.
</p>


<h3>Examples</h3>

```R
# The following is a toy example of a simulation, illustrating the use of
# the run function.
sim <- new_sim()
sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
sim %<>% add_method("estimator_2", function(dat) { var(dat) })
sim %<>% set_levels(
  "n" = c(10, 100, 1000),
  "estimator" = c("estimator_1", "estimator_2")
)
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- use_method(L$estimator, list(dat))
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim$results %>% print()
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
