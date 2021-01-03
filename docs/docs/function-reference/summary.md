---
layout: page
title: summary 
nav_order: 10 
permalink: /summary/
parent: Function reference
---

<table width="100%" summary="page for summary {simba}"><tr>
<td>summary {simba}</td>
<td style="text-align: right;">R Documentation</td>
</tr></table>

<h2>Summarize simulation results</h2>
<h3>Description</h3>
<p>Calculate summary statistics for simulation results
</p>
<h3>Usage</h3>
```R
summary(sim_obj, ...)
```
<h3>Arguments</h3>
<table summary="R argblock">
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td>
</tr>
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sd</span></td>
<td>
<p>If 'sd=TRUE' is passed, standard deviations are reported in
addition to means</p>
</td>
</tr>
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>coverage</span></td>
<td>
<p>!!!!! TO DO</p>
</td>
</tr>
</table>

<h3>Value</h3>
<p>!!!!! TO DO
</p>
<h3>Examples</h3>
```R
# The following is a toy example of a simulation, illustrating the use of
# the summary function.
sim <- new_sim()
sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })
sim %<>% add_method("estimator_1", function(dat) { mean(dat) })
sim %<>% add_method("estimator_2", function(dat) { var(dat) })
sim %<>% set_levels(
  "n" = c(10, 100, 1000),
  "estimator" = c("estimator_1", "estimator_2")
)
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- do.call(L$estimator, list(dat))
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summary(
  mean = list(name="mean_lambda_hat", x="lambda_hat"),
  mse = list(name="lambda_mse", estimate="lambda_hat", truth=5)
)
```
<hr>

<div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
