---
layout: page
title: get_complex 
nav_order: 2 
permalink: /function-reference/get_complex/
parent: Function reference
---

<script type="text/javascript">
const macros = { "\\R": "\\textsf{R}", "\\code": "\\texttt"};
function processMathHTML() {
    var l = document.getElementsByClassName('reqn');
    for (let e of l) { katex.render(e.textContent, e, { throwOnError: false, macros }); }
    return;
}</script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.js"
    onload="processMathHTML();"></script>
<link rel="stylesheet" type="text/css" href="R.css" />
</head><body><div class="container">

<table style="width: 100%;"><tr><td>get_complex {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Access internal simulation variables</h2>

<h3>Description</h3>

<p>Extract complex simulation data from a simulation object
</p>


<h3>Usage</h3>

```R<code class='language-R'>get_complex(sim, sim_uid)
</span>```


<h3>Arguments</h3>

<table>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span></p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span></td>
<td>
<p>The unique identifier of a single simulation replicate. This
corresponds to the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span> column in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>.</p>
</td></tr>
</table>


<h3>Value</h3>

<p>The value of the complex simulation result data corresponding to the
supplied <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_uid</span>
</p>


<h3>Examples</h3>

```R<code class='language-R'>sim <- new_sim()
create_data <- function(n) {
  x <- runif(n)
  y <- 3 + 2*x + rnorm(n)
  return(data.frame("x"=x, "y"=y))
}
sim %<>% set_levels("n"=c(10, 100, 1000))
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  model <- lm(y~x, data=dat)
  return (list(
    "beta1_hat" = model$coefficients[[2]],
    ".complex" = model
  ))
})
sim %<>% run()
sim$results %>% print()
get_complex(sim, 1) %>% print()
</span>```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.1.0 ]</div>
</div>
