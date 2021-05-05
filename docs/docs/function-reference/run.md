---
layout: page
title: run 
nav_order: 5 
permalink: /function-reference/run/
parent: Function reference
---


<table width="100%" summary="page for run {simba}"><tr><td>run {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Run the simulation</h2>

<h3>Description</h3>

<p>!!!!! TO DO
</p>


<h3>Usage</h3>

```R
run(sim_obj, sim_uids = NA)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uids</span></td>
<td>
<p>A vector of sim_uids that represent simulations to run. If
omitted, all simulations are run. # update this !!!!!</p>
</td></tr>
</table>


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
  lambda_hat <- do.call(L$estimator, list(dat))
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim$results
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
