---
layout: page
title: set_script 
nav_order: 8 
permalink: /function-reference/set_script/
parent: Function reference
---


<table width="100%" summary="page for set_script {SimEngine}"><tr><td>set_script {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Set the &quot;simulation script&quot;</h2>

<h3>Description</h3>

<p>Specify a function to be used as the &quot;simulation script&quot;. The
simulation script is a function that runs a single simulation replicate
and returns the results.
</p>


<h3>Usage</h3>

```R
set_script(sim, fn)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span></p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span></td>
<td>
<p>A function that runs a single simulation replicate and returns the
results. The results must be a list of key-value pairs. Values are
categorized as simple (a number, a character string, etc.) or complex
(vectors, dataframes, lists, etc.). Complex data must go inside a key
called &quot;.complex&quot; and the associated value must be a list (see examples).
The function body can contain references to the special object <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>L</span>
that stores the current set of simulation levels (see examples).
The keys must be valid R names (see ?make.names).</p>
</td></tr>
</table>


<h3>Value</h3>

<p>The original simulation object with the new &quot;simulation script&quot;
function added.
</p>


<h3>Examples</h3>

```R
# The following is a toy example of a simulation, illustrating the use of
# the set_script function.
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat, type=L$est)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim$results

# If you need to return complex result data (vectors, dataframes, lists,
# etc.), use the construct ".complex"=list().
sim <- new_sim()
sim %<>% set_levels(n=c(4,9))
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  dat <- rnorm(L$n)
  mtx <- matrix(dat, nrow=sqrt(length(dat)))
  return (list(
    "mean" = mean(dat),
    "det" = det(mtx),
    ".complex" = list(dat=dat, mtx=mtx)
  ))
})
sim %<>% run()

```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
