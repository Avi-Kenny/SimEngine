# Summarize simulation results

This function calculates summary statistics for simulation results,
including descriptive statistics (e.g. measures of center or spread) and
inferential statistics (e.g. bias or confidence interval coverage). All
summary statistics are calculated over simulation replicates within a
single simulation level.

## Usage

``` r
summarize(sim, ..., mc_se = FALSE)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)

- ...:

  One or more lists, separated by commas, specifying desired summaries
  of the `sim` simulation object. See examples. Each list must have a
  `stat` item, which specifies the type of summary statistic to be
  calculated. The `na.rm` item indicates whether to exclude `NA` values
  when performing the calculation (with default being `FALSE`). For
  `stat` options where the `name` item is optional, if it is not
  provided, a name will be formed from the type of summary and the
  column on which the summary is performed. Additional required items
  are detailed below for each `stat` type.

  - `list(stat="mean", x="col_1", name="mean_col", na.rm=F)` computes
    the mean of column `sim$results$col_1` for each scenario and creates
    a summary column named `"mean_col"`. Other single-column summary
    statistics (see the next few items) work analogously. `name` is
    optional.

  - `list(stat="median", ...)` computes the median.

  - `list(stat="var", ...)` computes the variance.

  - `list(stat="sd", ...)` computes the standard deviation.

  - `list(stat="mad", ...)` computes the mean absolute deviation.

  - `list(stat="iqr", ...)` computes the interquartile range.

  - `list(stat="min", ...)` computes the minimum.

  - `list(stat="max", ...)` computes the maximum.

  - `list(stat="is_na", ...)` computes the number of NA values.

  - `list(stat="correlation", x="col_1", y="col_2", name="cor_12")`
    computes the (Pearson's) correlation coefficient between
    `sim$results$col_1` and `sim$results$col_2` for each scenario and
    creates a summary column named `"cor_12"`.

  - `list(stat="covariance", x="col_1", y="col_2", name="cov_12")`
    computes the covariance between `sim$results$col_1` and
    `sim$results$col_2` for each scenario and creates a summary column
    named `"cov_12"`.

  - `list(stat="quantile", x="col_1", prob=0.8, name="q_col_1")`
    computes the 0.8 quantile of column `sim$results$col_1` and creates
    a summary column named `"q_col_1"`. `prob` can be any number in
    \[0,1\].

  - `list(stat="bias", estimate="est", truth=5, name="bias_est")`
    computes the absolute bias of the estimator corresponding to column
    `"sim$results$est"`, relative to the true value given in `truth`,
    and creates a summary column named `"bias_est"`. `name` is optional.
    See *Details*.

  - `list(stat="bias_pct", estimate="est", truth=5, name="bias_est")`
    computes the percent bias of the estimator corresponding to column
    `"sim$results$est"`, relative to the true value given in `truth`,
    and creates a summary column named `"bias_pct_est"`. `name` is
    optional. See *Details*.

  - `list(stat="mse", estimate="est", truth=5, name="mse_est")` computes
    the mean squared error of the estimator corresponding to column
    `"sim$results$est"`, relative to the true value given in `truth`,
    and creates a summary column named `"mse_est"`. `name` is optional.
    See *Details*.

  - `list(stat="mae", estimate="est", truth=5, name="mae_est")` computes
    the mean absolute error of the estimator corresponding to column
    `"sim$results$est"`, relative to the true value given in `truth`,
    and creates a summary column named `"mae_est"`. `name` is optional.
    See *Details*.

  - `list(stat="coverage", estimate="est", se="se_est", truth=5, name="cov_est")`
    or
    `list(stat="coverage", lower="est_l", upper="est_u", truth=5, name="cov_est")`
    computes confidence interval coverage. With the first form,
    `estimate` gives the name of the variable in `sim$results`
    corresponding to the estimator of interest and `se` gives the name
    of the variable containing the standard error of the estimator of
    interest. With the second form, `lower` gives the name of the
    variable containing the confidence interval lower bound and `upper`
    gives the name of the confidence interval upper bound. In both
    cases, `truth` is the true value (see *Details*), and a summary
    column named `"cov_est"` is created.

- mc_se:

  A logical argument indicating whether to compute Monte Carlo standard
  error and associated confidence interval for inferential summary
  statistics. This applies only to the `bias`, `bias_pct`, `mse`, `mae`,
  and `coverage` summary statistics.

## Value

A data frame containing the result of each specified summary function as
a column, for each of the simulation levels. The column `n_reps` returns
the number of successful simulation replicates within each level.

## Details

- For all inferential summaries there are three ways to specify
  `truth`: (1) a single number, meaning the estimand is the same across
  all simulation replicates and levels, (2) a numeric vector of the same
  length as the number of rows in `sim$results`, or (3) the name of a
  variable in `sim$results` containing the estimand of interest.

- There are two ways to specify the confidence interval bounds for
  `coverage`. The first is to provide an `estimate` and its associated
  `se` (standard error). These should both be variables in
  `sim$results`. The function constructs a 95% Wald-type confidence
  interval of the form `(estimate-1.96*se, estimate+1.96*se)`. The
  alternative is to provide `lower` and `upper` bounds, which should
  also be variables in `sim$results`. In this case, the confidence
  interval is (`lower`, `upper`). The coverage is the proportion of
  simulation replicates for a given scenario in which `truth` lies
  within the interval.

## Examples

``` r
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
