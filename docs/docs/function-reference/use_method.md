---
layout: page
title: use_method 
nav_order: 15 
permalink: /function-reference/use_method/
parent: Function reference
---


<table width="100%" summary="page for use_method {SimEngine}"><tr><td>use_method {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Use a method</h2>

<h3>Description</h3>

<p>This function calls the specified method, passing along any
arguments that have been specified in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>args</span>. It will typically be
used in conjunction with the special object L to dynamically run methods
that have been included as simulation levels. This function is a wrapper
around do.call and is used in a similar manner. See examples.
</p>


<h3>Usage</h3>

```R
use_method(method, args = list())
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>method</span></td>
<td>
<p>A character string naming a function that has been added to
your simulation object via <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>add_method</span></p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>args</span></td>
<td>
<p>A list of arguments to be passed onto <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>method</span></p>
</td></tr>
</table>


<h3>Value</h3>

<p>The result of the call to <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>method</span>
</p>


<h3>Examples</h3>

```R
# The following is a toy example of a simulation, illustrating the use of
# the use_method function.
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
sim$results
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
