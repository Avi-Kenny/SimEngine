# Advanced functionality

## Complex simulation levels

Often, simulation levels will be simple, such as a vector of sample
sizes:

``` r
sim <- new_sim()
sim %<>% set_levels(n = c(200,400,800))
```

However, there are many instances in which more complex objects are
needed. For these cases, instead of a vector of numbers or character
strings, a named list of lists can be used. The toy example below
illustrates this.

``` r
sim <- new_sim()
sim %<>% set_levels(
  n = c(10,100),
  distribution = list(
    "Beta 1" = list(type="Beta", params=c(0.3, 0.7)),
    "Beta 2" = list(type="Beta", params=c(1.5, 0.4)),
    "Normal" = list(type="Normal", params=c(3.0, 0.2))
  )
)
create_data <- function(n, type, params) {
  if (type=="Beta") {
    return(rbeta(n, shape1=params[1], shape2=params[2]))
  } else if (type=="Normal") {
    return(rnorm(n, mean=params[1], sd=params[2]))
  }
}
sim %<>% set_script(function() {
  x <- create_data(L$n, L$distribution$type, L$distribution$params)
  return(list("y"=mean(x)))
})
sim %<>% run()
#> Done. No errors or warnings detected.
```

Note that the list names (`"Beta 1"`, `"Beta 2"`, and `"Normal"`) become
the entries in the `sim$results` dataframe, as well as the dataframe
returned by
[`summarize()`](https://avi-kenny.github.io/SimEngine/reference/summarize.md).

``` r
sim %>% summarize(list(stat="mean", x="y"))
#>   level_id   n distribution n_reps    mean_y
#> 1        1  10       Beta 1      1 0.3974106
#> 2        2 100       Beta 1      1 0.3097869
#> 3        3  10       Beta 2      1 0.7026055
#> 4        4 100       Beta 2      1 0.7844416
#> 5        5  10       Normal      1 2.9276107
#> 6        6 100       Normal      1 2.9799967
```

Sometimes, it may be the case that you want to run simulations only for
a subset of scenarios (i.e., not for all combinations of level values).
In these cases, use the `.keep` option of
[`set_levels()`](https://avi-kenny.github.io/SimEngine/reference/set_levels.md)
. First, set the levels normally. Second, view the `sim$levels_grid`
dataframe to examine the scenarios and the associated `level_id` values.
Third, call
[`set_levels()`](https://avi-kenny.github.io/SimEngine/reference/set_levels.md)
again with the `.keep` option to specify which scenarios to keep. This
is demonstrated below:

``` r
sim <- new_sim()
sim %<>% set_levels(alpha=c(2,3,4), beta=c(50,60))
print(sim$levels_grid)
#>   level_id alpha beta
#> 1        1     2   50
#> 2        2     3   50
#> 3        3     4   50
#> 4        4     2   60
#> 5        5     3   60
#> 6        6     4   60
sim %<>% set_levels(.keep=c(1,2,6))
print(sim$levels_grid)
#>   level_id alpha beta
#> 1        1     2   50
#> 2        2     3   50
#> 6        6     4   60
```

## Complex return data

In most situations, the results of simulations are numeric. However, we
may want to return more complex data, such as matrices, lists, or model
objects. To do this, we add a key-value pair to the list returned by the
simulation script with the special key `".complex"` and a list
(containing the complex data) as the value. This is illustrated in the
toy example below. Here, the simulation script estimates the parameters
of a linear regression and returns these as numeric, but also returns
the estimated covariance matrix and the entire model object.

``` r
sim <- new_sim()
sim %<>% set_levels(n=c(10, 100, 1000))
create_data <- function(n) {
  x <- runif(n)
  y <- 3 + 2*x + rnorm(n)
  return(data.frame("x"=x, "y"=y))
}
sim %<>% set_config(num_sim=2)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  model <- lm(y~x, data=dat)
  return(list(
    "beta0_hat" = model$coefficients[[1]],
    "beta1_hat" = model$coefficients[[2]],
    ".complex" = list(
      "model" = model,
      "cov_mtx" = vcov(model)
    )
  ))
})
sim %<>% run()
#> Done. No errors or warnings detected.
```

After running this simulation, the numeric results can be accessed
directly via `sim$results` or using the
[`summarize()`](https://avi-kenny.github.io/SimEngine/reference/summarize.md)
function, as usual:

``` r
head(sim$results)
#>   sim_uid level_id rep_id    n     runtime beta0_hat  beta1_hat
#> 1       1        1      1   10 0.003092051  4.357518 -0.8661117
#> 2       4        1      2   10 0.001080513  1.796235  3.7833815
#> 3       2        2      1  100 0.004112959  2.648096  2.2148375
#> 4       5        2      2  100 0.001054525  3.095621  1.7386195
#> 5       3        3      1 1000 0.001422644  3.076829  1.9557942
#> 6       6        3      2 1000 0.001262665  2.964774  2.0288481
```

To examine the complex return data, we can use the function
[`get_complex()`](https://avi-kenny.github.io/SimEngine/reference/get_complex.md),
as illustrated below:

``` r
c5 <- get_complex(sim, sim_uid=5)
print(summary(c5$model))
#> 
#> Call:
#> lm(formula = y ~ x, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2.35932 -0.75445 -0.05861  0.59426  2.61694 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   3.0956     0.2087  14.831  < 2e-16 ***
#> x             1.7386     0.3766   4.617 1.18e-05 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.031 on 98 degrees of freedom
#> Multiple R-squared:  0.1786, Adjusted R-squared:  0.1702 
#> F-statistic: 21.31 on 1 and 98 DF,  p-value: 1.185e-05
print(c5$cov_mtx)
#>             (Intercept)           x
#> (Intercept)  0.04356769 -0.06834654
#> x           -0.06834654  0.14182987
```

To process all complex data, loop through the `sim_uids`, as follows:

``` r
for (i in sim$results$sim_uid) {
  cmplx <- get_complex(sim, sim_uid=i)
  print(as.numeric(diag(vcov(cmplx$model))))
}
#> [1] 0.4885532 1.4577681
#> [1] 0.1322510 0.4510751
#> [1] 0.02732735 0.08726350
#> [1] 0.04356769 0.14182987
#> [1] 0.004105276 0.012528273
#> [1] 0.004032338 0.012030212
```

## Using the `batch` function

The
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function is useful for sharing data or objects between simulation
replicates. Essentially, it allows simulation replicates to be divided
into “batches”; all replicates in a given batch will then share a
certain set of objects. A common use case for this is a simulation that
involves using multiple methods to analyze a shared dataset, and
repeating this process over a number of dataset replicates. This may be
of interest if, for example, it is computationally expensive to generate
a simulated dataset.

To illustrate the use of
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
using this example, we first consider the following simulation:

``` r
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3)
sim %<>% set_script(function() {
  dat <- create_data(n=100)
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()
#> Done. No errors or warnings detected.
```

From the `"dat_1"` column of the results object (equal to the first
element of the `dat` vector created in the simulation script), we see
that a unique dataset was created for each simulation replicate:

``` r
sim$results[order(sim$results$rep_id),c(1:7)!=5]
#>   sim_uid level_id rep_id        est mu_hat dat_1
#> 1       1        1      1   est_mean   2.97  1.49
#> 4       2        2      1 est_median   3.04  0.63
#> 2       3        1      2   est_mean   3.01  4.57
#> 5       5        2      2 est_median   2.95  0.57
#> 3       4        1      3   est_mean   2.99  1.99
#> 6       6        2      3 est_median   3.21  2.09
```

Suppose that instead, we wish to analyze each simulated dataset using
multiple methods (in this case corresponding to `"est_mean"` and
`"est_median"`), and repeat this procedure a total of three times. We
can do this using the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function, as follows:

``` r
sim <- new_sim()
create_data <- function(n) { rnorm(n=n, mean=3) }
est_mean <- function(dat, type) {
  if (type=="est_mean") { return(mean(dat)) }
  if (type=="est_median") { return(median(dat)) }
}
sim %<>% set_levels(est=c("est_mean","est_median"))
sim %<>% set_config(num_sim=3, batch_levels=NULL)
sim %<>% set_script(function() {
  batch({
    dat <- create_data(n=100)
  })
  mu_hat <- est_mean(dat=dat, type=L$est)
  return(list(
    "mu_hat" = round(mu_hat,2),
    "dat_1" = round(dat[1],2)
  ))
})
sim %<>% run()
#> Done. No errors or warnings detected.
```

In the code above, we changed two things. First, we added
`batch_levels=NULL` to the
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
call; this will be explained below. Second, we wrapped the code line
`dat <- create_data(n=100)` inside the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function. Whatever code goes inside the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function will produce the same output for all simulations in a batch.

``` r
sim$results[order(sim$results$rep_id),c(1:7)!=5]
#>   sim_uid level_id rep_id        est mu_hat dat_1
#> 1       1        1      1   est_mean   2.85  3.58
#> 4       2        2      1 est_median   2.83  3.58
#> 2       3        1      2   est_mean   2.98  2.89
#> 5       5        2      2 est_median   3.07  2.89
#> 3       4        1      3   est_mean   2.92  3.70
#> 6       6        2      3 est_median   2.98  3.70
```

In this case, from the `"dat_1"` column of the results object, we see
that one dataset was created and shared by the batch corresponding to
`sim_uids` 1 and 2 (likewise for `sim_uids` {3,5} and {4,6}).

However, the situation is often more complicated. Suppose we have a
simulation with multiple levels, some that correspond to creating data
and some that correspond to analyzing the data. Here, the `batch_levels`
argument of
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
plays a role. Specifically, this argument should be a character vector
equal to the names of the simulation levels that are referenced (via the
special variable `L`) from within a
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
block. In the example below, the levels `n` and `mu` are used within the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
call, while the level `est` is not.

``` r
sim <- new_sim()
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
#> Done. No errors or warnings detected.

sim$results[order(sim$results$batch_id),c(1:10)!=8]
#>    sim_uid level_id rep_id batch_id   n mu        est mu_hat dat_1
#> 1        1        1      1        1  10  3   est_mean   2.55  1.72
#> 9        5        5      1        1  10  3 est_median   2.67  1.72
#> 2        9        1      2        2  10  3   est_mean   3.68  2.11
#> 10      13        5      2        2  10  3 est_median   3.82  2.11
#> 3        2        2      1        3 100  3   est_mean   2.80  3.77
#> 11       6        6      1        3 100  3 est_median   2.92  3.77
#> 4       10        2      2        4 100  3   est_mean   2.94  2.78
#> 12      14        6      2        4 100  3 est_median   2.93  2.78
#> 5        3        3      1        5  10  5   est_mean   5.17  4.64
#> 13       7        7      1        5  10  5 est_median   5.27  4.64
#> 6       11        3      2        6  10  5   est_mean   5.59  4.45
#> 14      15        7      2        6  10  5 est_median   5.52  4.45
#> 7        4        4      1        7 100  5   est_mean   5.00  5.86
#> 15       8        8      1        7 100  5 est_median   4.93  5.86
#> 8       12        4      2        8 100  5   est_mean   5.14  5.25
#> 16      16        8      2        8 100  5 est_median   5.21  5.25
```

The batches were created such that each batch contained two replicates,
one for each level value of `est`. For expository purposes, we also
specified the `return_batch_id=T` option in
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
so that the results object would return the `batch_id`. This is not
necessary in practice. The `batch_id` variable defines the batches; all
simulations that share the same `batch_id` are in a single batch. The
`return_batch_id=T` option can be useful to ensure correct usage of the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function.

We note the following about the
[`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
function:

- The code within the
  [`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
  code block must *only* create objects; this code should not change or
  delete existing objects, as these changes will be ignored.
- In the majority of cases, the
  [`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
  function will be called just once, at the beginning of the simulation
  script. However, it can be used anywhere in the script and can be
  called multiple times. The
  [`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
  function should never be used outside of the simulation script.
- Although we have illustrated the use of the
  [`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
  function to create a dataset to share between multiple simulation
  replicates, it can be used for much more, such as taking a sample from
  an existing dataset or computing shared nuisance function estimators.
- If the simulation is being run in parallel (either locally or on a
  CCS), `n_cores` cannot exceed the number of batches, since all
  simulations within a batch must run on the same core.
- If the simulation script uses the
  [`batch()`](https://avi-kenny.github.io/SimEngine/reference/batch.md)
  function, the simulation cannot be updated using the
  [`update_sim()`](https://avi-kenny.github.io/SimEngine/reference/update_sim.md)
  or
  [`update_sim_on_cluster()`](https://avi-kenny.github.io/SimEngine/reference/update_sim_on_cluster.md)
  functions, with the exception of updates that only entail removing
  simulation replicates.

## Random number generator seeds

In statistical research, it is desirable to be able to reproduce the
exact results of a simulation study. Since R code often involves
stochastic (random) functions like
[`rnorm()`](https://rdrr.io/r/stats/Normal.html) or
[`sample()`](https://rdrr.io/r/base/sample.html) that return different
values when called multiple times, reproducibility is not guaranteed. In
a simple R script, calling the
[`set.seed()`](https://rdrr.io/r/base/Random.html) function at the
beginning of the script ensures that the stochastic functions that
follow will produce the same results whenever the script is run.
However, a more nuanced strategy is needed when running simulations.
When running 100 replicates of the same simulation, we do not want each
replicate to return identical results; rather, we would like for each
replicate to be different from one another, but for *the entire set of
replicates* to be the same when the entire simulation is run twice in a
row. **SimEngine** manages this process, even when simulations are being
run in parallel locally or on a cluster computing system. In
**SimEngine**, a single “global seed” is used to generate a different
seed for each simulation replicate. The
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
function is used to set or change this global seed:

``` r
sim %<>% set_config(seed=123)
```

If a seed is not set using
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md),
**SimEngine** will set a random seed automatically so that the results
can be replicated if desired. To view this seed, we use the
[`vars()`](https://avi-kenny.github.io/SimEngine/reference/vars.md)
function:

``` r
sim <- new_sim()
print(vars(sim, "seed"))
#> [1] 287577520
```

## Debugging and error/warning handling

In the simulation coding workflow, errors are inevitable. Some errors
may affect all simulation replicates, while other errors may only affect
a subset of replicates. By default, when a simulation is run,
**SimEngine** will not stop if an error occurs; instead, errors are
logged and stored in a dataframe along with information about the
simulation replicates that resulted in those errors. Examining this
dataframe by typing `print(sim$errors)` can sometimes help to quickly
pinpoint the issue. This is demonstrated below:

``` r
sim <- new_sim()
sim %<>% set_config(num_sim=2)
sim %<>% set_levels(
  Sigma = list(
    s1 = list(mtx=matrix(c(3,1,1,2), nrow=2)),
    s3 = list(mtx=matrix(c(4,3,3,9), nrow=2)),
    s2 = list(mtx=matrix(c(1,2,2,1), nrow=2)),
    s4 = list(mtx=matrix(c(8,2,2,6), nrow=2))
  )
)
sim %<>% set_script(function() {
  x <- MASS::mvrnorm(n=1, mu=c(0,0), Sigma=L$Sigma$mtx)
  return(list(x1=x[1], x2=x[2]))
})
sim %<>% run()
#> Done. Errors detected in 25% of simulation replicates. Warnings detected in 0% of simulation replicates.
print(sim$errors)
#>   sim_uid level_id rep_id Sigma      runtime                          message
#> 1       5        3      1    s2 0.0003919601 'Sigma' is not positive definite
#> 2       6        3      2    s2 0.0003745556 'Sigma' is not positive definite
#>                                                      call
#> 1 MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma$mtx)
#> 2 MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma$mtx)
```

From the output above, we see that the code fails for the simulation
replicates that use the level with `Sigma="s2"` because it uses an
invalid (not positive definite) covariance matrix. Similarly, if a
simulation involves replicates that throw warnings, all warnings are
logged and stored in the dataframe `sim$warnings`. If an error occurs
for a subset of simulation replicates and that error is fixed, it is
possible to rerun the replicates that had errors, as follows:

``` r
sim %<>% update_sim(keep_errors=FALSE)
```

The workflow demonstrated above can be useful to pinpoint errors, but it
has two main drawbacks. First, it is undesirable to run a time-consuming
simulation involving hundreds or thousands of replicates, only to find
at the end that every replicate failed because of a typo. It may
therefore useful to stop an entire simulation after a single error has
occurred. Second, it can sometimes be difficult to determine exactly
what caused an error without making use of more advanced debugging
tools. For both of these situations, **SimEngine** includes the
following configuration option:

``` r
sim %<>% set_config(stop_at_error=TRUE)
```

Setting `stop_at_error=TRUE` will stop the simulation when it encounters
any error. Furthermore, the error will be thrown by R in the usual way,
so if the simulation is being run in in RStudio, the built-in debugging
tools (such as “Show Traceback” and “Rerun with debug”) can be used to
find and fix the bug. Placing a call to
[`browser()`](https://rdrr.io/r/base/browser.html) at the top of the
simulation script can also be useful for debugging.

## Monte Carlo error

Statistical simulations are often based on the principle of Monte Carlo
approximation; specifically, pseudo-random sampling is used to evaluate
the performance of a statistical procedure under a particular
data-generating process. The performance of the procedure can be viewed
as a statistical parameter and, due to the fact that only a finite
number of simulation replicates can be performed, there is uncertainty
in any estimate of performance. This uncertainty is often referred to as
*Monte Carlo error*. We can quantify Monte Carlo error using, for
example, the standard error of the performance estimator.

Measuring and reporting Monte Carlo error is a vital component of a
simulation study. **SimEngine** includes an option in the
[`summarize()`](https://avi-kenny.github.io/SimEngine/reference/summarize.md)
function to automatically estimate the Monte Carlo standard error for
any inferential summary statistic, e.g., estimator bias or confidence
interval coverage. The standard error estimates are based on the
formulas provided in Morris et al. 2019. If the option `mc_se` is set to
`TRUE`, estimates of Monte Carlo standard error will be included in the
summary data frame, along with associated 95% confidence intervals based
on a normal approximation.

``` r
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat) {
  return(mean(dat))
}
sim %<>% set_levels(n=c(10,100,1000))
sim %<>% set_config(num_sim=5)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
#> Done. No errors or warnings detected.
sim %>% summarize(
  list(stat="mse", name="lambda_mse", estimate="lambda_hat", truth=5), 
  mc_se = TRUE
)
#>   level_id    n n_reps lambda_mse lambda_mse_mc_se lambda_mse_mc_ci_l
#> 1        1   10      5   0.602000     0.4280700877       -0.237017372
#> 2        2  100      5   0.052200     0.0234607545        0.006216921
#> 3        3 1000      5   0.002099     0.0005030319        0.001113057
#>   lambda_mse_mc_ci_u
#> 1        1.441017372
#> 2        0.098183079
#> 3        0.003084943
```
