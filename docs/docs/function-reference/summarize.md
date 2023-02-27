---
layout: page
title: summarize 
nav_order: 10 
permalink: /function-reference/summarize/
parent: Function reference
---

<table style="width: 100%;"><tr><td>summarize {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Summarize simulation results</h2>

<h3>Description</h3>

<p>This function calculates summary statistics for simulation results,
including descriptive statistics (e.g. measures of
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
<p>One or more lists, separated by commas, specifying desired summaries of the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span>
simulation object. See examples. Each list must have a <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> item, which specifies the type of summary statistic to
be calculated. The <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>na.rm</span> item indicates whether to exclude <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>NA</span> values when performing the calculation (with
default being <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>FALSE</span>). For <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> options where the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> item is optional,
if it is not provided, a name will be formed from the type of summary and the column on which the summary
is performed. Additional required items are detailed below for each <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>stat</span> type.
</p>

<ul>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="mean", x="col_1", name="mean_col")</span> computes the
mean of column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_1</span> for each level combination and
creates a summary column named <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"mean_col"</span>. Other single-column
summary statistics (see the next few items) work analogously. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span>
is optional.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="median", ...)</span> computes the median.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="var", ...)</span> computes the variance.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="sd", ...)</span> computes the standard deviation.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="mad", ...)</span> computes the mean absolute deviation.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="iqr", ...)</span> computes the interquartile range.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="min", ...)</span> computes the minimum.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="max", ...)</span> computes the maximum.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="is_na", ...)</span> computes the number of NA values.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="correlation", x="col_1", y="col_2",
    name="cor_12")</span> computes the (Pearson's) correlation coefficient between
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_1</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_2</span> for each level
combination and creates a summary column named <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"cor_12"</span>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="covariance", x="col_1", y="col_2",
    name="cov_12")</span> computes the covariance between <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_1</span>
and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_2</span> for each level combination and creates a
summary column named <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"cov_12"</span>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="quantile", x="col_1", prob=0.8, name="q_col_1")</span>
computes the 0.8 quantile of column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results$col_1</span> and creates
a summary column named <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"q_col_1"</span>. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>prob</span> can be any number in
[0,1].
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="bias", estimate="est", truth=5,
    name="bias_est")</span> computes the absolute bias of the estimator
corresponding to column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"sim$results$est"</span>, relative to the true
value given in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span>, and creates a summary column named
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"bias_est"</span>. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> is optional. See <em>Details</em>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="bias_pct", estimate="est", truth=5,
    name="bias_est")</span> computes the percent bias of the estimator
corresponding to column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"sim$results$est"</span>, relative to the true
value given in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span>, and creates a summary column named
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"bias_pct_est"</span>. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> is optional. See <em>Details</em>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="mse", estimate="est", truth=5,
    name="mse_est")</span> computes the mean squared error of the estimator
corresponding to column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"sim$results$est"</span>, relative to the true
value given in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span>, and creates a summary column named
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"mse_est"</span>. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> is optional. See <em>Details</em>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="mae", estimate="est", truth=5,
    name="mae_est")</span> computes the mean absolute error of the estimator
corresponding to column <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"sim$results$est"</span>, relative to the true
value given in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span>, and creates a summary column named
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"mae_est"</span>. <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span> is optional. See <em>Details</em>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="coverage", estimate="est", se="se_est",
    truth=5, name="cov_est")</span> or
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>list(stat="coverage", lower="est_l", upper="est_u",
    truth=5, name="cov_est")</span> computes confidence interval coverage. With the
first form, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimate</span> gives the name of the variable in
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span> corresponding to the estimator of interest and
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>se</span> gives the name of the variable containing the standard error of
the estimator of interest. With the second form, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span> gives the
name of the variable containing the confidence interval lower bound and
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span> gives the name of the confidence interval upper bound. In
both cases, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> is the true value (see <em>Details</em>), and a
summary column named <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>"cov_est"</span> is created.
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
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>(estimate-1.96*se, estimate+1.96*se)</span>. The alternative is to provide
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span> bounds, which should also be variables in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$results</span>. In this case,
the confidence interval is (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>lower</span>, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>upper</span>). The coverage is the proportion of simulation
replicates for a given level combination in which <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>truth</span> lies within the interval.
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
