---
layout: page
title: batch 
nav_order: 1 
permalink: /function-reference/batch/
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

<table style="width: 100%;"><tr><td>batch {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Run a block of code as part of a batch</h2>

<h3>Description</h3>

<p>This function is designed to be used within a simulation script
to leverage &quot;replicate batches&quot;. This is useful if you want to share data
or objects between simulation replicates. Essentially, it allows you to
take your simulation replicates and divide them into &quot;batches&quot;; all
replicates in a given batch will then share a single set of objects. The
most common use case for this is if you have a simulation that involves
generating one dataset, analyzing it using multiple methods, and then
repeating this a number of times. See
<a href="https://avi-kenny.github.io/SimEngine/advanced-usage/#using-the-batch-function">https://avi-kenny.github.io/SimEngine/advanced-usage/#using-the-batch-function</a>
for a thorough overview of how this function is used.
</p>


<h3>Usage</h3>

```R<code class='language-R'>batch(code)
</span>```


<h3>Examples</h3>

```R<code class='language-R'>sim <- new_sim()
create_data <- function(n, mu) { rnorm(n=n, mean=mu) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(n=c(10,100), mu=c(3,5), est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=2, batch_levels=c("n", "mu"), return_batch_id=T)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=L$n, mu=L$mu)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()
sim$results[order(sim$results$batch_id),]
</span>```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.1.0 ]</div>
</div>
