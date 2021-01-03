---
layout: page
title: set_script 
nav_order: 9 
permalink: /set_script/
parent: Function reference
---

<table width="100%" summary="page for set_script {simba}"><tr>
<td>set_script {simba}</td>
<td style="text-align: right;">R Documentation</td>
</tr></table>
<h2>Set the "simulation script"</h2><h3>Description</h3><p>Specify a function to be used as the "simulation script". The
simulation script is a function that runs a single simulation replicate
and returns the results.
</p><h3>Usage</h3>```R
set_script(sim_obj, fn)
```<h3>Arguments</h3><table summary="R argblock">
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td>
</tr>
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span></td>
<td>
<p>A function that runs a single simulation replicate and returns the
result. The result must be a list of key-value pairs. The values
themselves can either be simple (numeric, character, etc.) or complex
(matrices, lists, etc.). The function body can contain references to the
special objects 'L' (simulation levels) and 'C' (simulation constants).
See examples.</p>
</td>
</tr>
</table>
<h3>Value</h3><p>The original simulation object with the new "simulation script"
function added.
</p><h3>Examples</h3>```R
# The following is a toy example of a simulation, illustrating the use of
# the set_script function.
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
```<hr>
<div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>