---
layout: page
title: summarize 
nav_order: 11 
permalink: /function-reference/summarize/
parent: Function reference
---

<table style="width: 100%;"><tr><td>summarize {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Summarize simulation results</h2>

<h3>Description</h3>

<p>This function calculates summary statistics for simulation results.
Options for summary statistics include descriptive statistics (e.g. measures of
center or spread) and inferential statistics (e.g. bias or confidence interval
coverage). All summary statistics are calculated over simulation replicates
within a single simulation level.
</p>


<h3>Usage</h3>

```R
summarize(sim, ...)
```


<h3>Arguments</h3>

<table>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span></p>
</td></tr>
<tr style="vertical-align: top;"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>...</span></td>
<td>
<p>One or more named lists, separated by commas, specifying desired summaries of the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span>
simulation object. Each list must have a <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> item, which specifies the summary statistic to
be calculated. For all <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> options besides <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"coverage"</span>, the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> item is optional;
if it is not provided, a name will be formed from the type of summary and the column on which the summary
is performed. The <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>na.rm</span> item indicates whether to exclude <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>NA</span> values when performing the calculation (with
default being <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>). Additional required items are detailed below for each <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> type.
</p>

<ul>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "mean"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the mean.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "median"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the median.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "var"</span> (variance): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the variance.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "sd"</span> (standard deviation): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the standard deviation.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "mad"</span> (mean absolute deviation): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the MAD.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "iqr"</span> (interquartile range): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the IQR.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "min"</span> (minimum): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the minimum.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "max"</span> (maximum): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the maximum.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "quantile"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the quantile, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>prob</span> is a number in [0,1] denoting the desired quantile.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "correlation"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>y</span> give the names of the variables in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the (Pearson's) correlation.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "covariance"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>y</span> give the names of the variables in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the covariance.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "bias"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the estimand of interest (see <em>Details</em>).
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "bias_pct"</span> (percent bias): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the estimand of interest (see <em>Details</em>).
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "mse"</span> (mean squared error): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the estimand of interest (see <em>Details</em>).
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "mae"</span> (mean absolute error): <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the estimand of interest (see <em>Details</em>).
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "coverage"</span> (confidence interval coverage). Either
(<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span>) or (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span>) must be provided. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the standard error of the estimator of interest, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span> gives the name of the variable in
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span> containing the confidence interval lower bound, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span> gives the name of the
variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span> containing the confidence interval upper bound, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the
estimand of interest. See <em>Details</em>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat = "is_na"</span>: <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>x</span> gives the name of the variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
on which to calculate the number of <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>NA</span>s.
</p>
</li></ul>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>For all inferential summaries there are three ways to specify <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span>: (1) a single number,
meaning the estimand is the same across all simulation replicates and levels, (2) a numeric vector of the
same length as the number of rows in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>, or (3) the name of a variable in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>
containing the estimand of interest.
</p>
</li>
<li><p>There are two ways to specify the confidence interval bounds for <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>coverage</span>. The first is to provide
an <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> and its associated <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span> (standard error). These should both be variables in
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>. The function constructs a 95% Wald-type confidence interval of the form
(<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> - 1.96 <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> + 1.96 <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span>).  The alternative is to provide
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span> bounds, which should also be variables in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>. In this case,
the confidence interval is (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span>). The coverage is simply the proportion of simulation
replicates for a given level in which <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> lies within the interval.
</p>
</li></ul>



<h3>Value</h3>

<p>A data frame containing the result of each specified summary function as a column, for each of
the simulation levels. The column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>n_reps</span> returns the number of successful simulation replicates
within each level.
</p>


<h3>Examples</h3>

```R
# The following is a toy example of a simulation, illustrating the use of
# the summarize function.
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat, type=L$est)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summarize(
  list(stat = "mean", name="mean_lambda_hat", x="lambda_hat"),
  list(stat = "mse", name="lambda_mse", estimate="lambda_hat", truth=5)
)
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.2.0 ]</div>
