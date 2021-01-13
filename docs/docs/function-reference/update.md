---
layout: page
title: update 
nav_order: 13 
permalink: /update/
parent: Function reference
---


<table width="100%" summary="page for update {simba}"><tr><td>update {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Update a simulation</h2>

<h3>Description</h3>

<p>This function updates a previously run simulation. After a simulation
has been run, you can alter the levels of the
resulting object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span> using set_levels, or change the configuration
(including the number of simulation replicates) using set_config. Executing
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>update</span> on this simulation object will only run the added levels/replicates,
without repeating anything that has already been run.
</p>


<h3>Usage</h3>

```R
update(sim_obj, keep_errors = TRUE, keep_extra = FALSE)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim, that has already been run by the run function</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_errors</span></td>
<td>
<p>logical (TRUE by default); if TRUE, do not try to re-run
simulation reps that results in errors previously; if FALSE, attempt to
run those reps again</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_extra</span></td>
<td>
<p>logical (FALSE by default); if TRUE, keep previously run
simulation reps even if they exceed the current <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim</span> in config or are from
a level that has been dropped; if FALSE, drop excess reps (starting from the last rep
for that particular simulation level)</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>It is not possible to add new level variables, only new levels of the
existing variables. Because of this, it is best practice to include all potential
level variables before initially running a simulation, even if some of them only
contain a single level. This way, additional levels can be added later.
</p>
</li>
<li> <p>In general, if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim</span> has been reduced prior to running <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>update</span>,
it is best to use the default option <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_extra = FALSE</span>. Otherwise, some
simulation levels will have more replicates than others, which makes comparison
difficult.
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with additional simulation replicates in
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>results</span> or <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>errors</span>
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
sim %<>% add_method("estimator_2", function(dat) { var(dat) })
sim %<>% set_levels(
  "n" = c(10, 100),
  "estimator" = c("estimator_1")
)
sim %<>% set_config(num_sim=10)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- do.call(L$estimator, list(dat))
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %<>% set_levels(
  "n" = c(10, 100, 1000),
  "estimator" = c("estimator_1", "estimator_2")
)
sim %<>% set_config(num_sim=5)
sim %<>% update()
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
