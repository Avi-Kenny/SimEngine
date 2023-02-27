---
layout: page
title: update_sim 
nav_order: 11 
permalink: /function-reference/update_sim/
parent: Function reference
---

<table style="width: 100%;"><tr><td>update_sim {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Update a simulation</h2>

<h3>Description</h3>

<p>This function updates a previously run simulation. After a
simulation has been <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>run</span>, you can alter the levels of the
resulting object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span> using <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_levels</span>,
or change the configuration (including the number of simulation
replicates) using <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config</span>. Executing <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>update_sim</span> on
this simulation object will only run the added levels/replicates, without
repeating anything that has already been run.
</p>


<h3>Usage</h3>

```R
update_sim(sim, keep_errors = T)
```


<h3>Arguments</h3>

<table>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span>, that has already been run by the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>run</span>
function</p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_errors</span></td>
<td>
<p>logical (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span> by default); if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>TRUE</span>, do not
try to re-run simulation reps that results in errors previously; if
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>, attempt to run those reps again</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>It is not possible to add new level variables, only new levels of the
existing variables. Because of this, it is best practice to include all
potential level variables before initially running a simulation, even if
some of them only contain a single level. This way, additional levels can
be added later.
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with additional simulation replicates
in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>results</span> or <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>errors</span>
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
sim %<>% set_levels(n=c(10,100), est="M")
sim %<>% set_config(num_sim=10)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat, type=L$est)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
sim %<>% set_config(num_sim=5)
sim %<>% update_sim()
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.2.0 ]</div>
