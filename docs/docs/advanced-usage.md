---
layout: page
title: Advanced usage
nav_order: 3
permalink: /advanced-usage/
---

# Advanced usage
{: .fs-9 }

---

## Complex simulation levels

Often, simulation levels will be simple, such as a vector of sample sizes:

```R
sim <- new_sim()
sim %<>% set_levels(
  n = c(200,400,800)
)
```

However, there will be many instances in which more complex objects are needed. For these cases, instead of a vector of numbers or character strings, use a *named* list of lists. The toy example below illustrates this. Note that the list names (`"Beta 1"`, `"Beta 2"`, and `"Normal"`) become the entries in the dataframe returned by `summarize`.

```R
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
#>   |########################################| 100%
#> Done. No errors or warnings detected.
sim %>% summarize()
#>   level_id   n distribution mean_runtime    mean_y
#> 1        1  10       Beta 1 0.0032717705 0.3411949
#> 2        2 100       Beta 1 0.0012259722 0.3052342
#> 3        3  10       Beta 2 0.0004872084 0.7922969
#> 4        4 100       Beta 2 0.0006912470 0.7854644
#> 5        5  10       Normal 0.0008846998 3.0360586
#> 6        6 100       Normal 0.0007148027 2.9898891
```

## Complex return data

In most situations, the results of simulations will be numeric. However, we may want to return more complex data, such as matrices, lists, or model objects. To do this, we include our complex return data in the list with the special key `".complex"`. This is illustrated in the toy example below, in which we estimate the parameters of a linear regression and returns these as numeric, but also return the estimated covariance matrix and the entire model object.

```R
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
  return (list(
    "beta0_hat" = model$coefficients[[1]],
    "beta1_hat" = model$coefficients[[2]],
    ".complex" = list(
	  "model" = model,
	  "cov_mtx" = vcov(model)
	)
  ))
})
sim %<>% run()
#>   |########################################| 100%
#> Done. No errors or warnings detected.
```

After running this simulation, we can examine the numeric results directly by accessing `sim$results` or using the `summarize` function, as usual:

```R
sim$results %>% print()
#>   sim_uid level_id rep_id    n      runtime beta0_hat beta1_hat
#> 1       1        1      1   10 0.0030298233  3.352146  2.070652
#> 2       2        1      2   10 0.0000000000  2.442415  3.557133
#> 3       3        2      1  100 0.0040059090  2.688359  2.564584
#> 4       4        2      2  100 0.0024149418  2.668047  2.542642
#> 5       5        3      1 1000 0.0075380802  3.019869  1.991019
#> 6       6        3      2 1000 0.0009970665  3.021231  2.026103
```

However, we may also want to look at the complex return data. To do so, we use the special function `get_complex`, as illustrated below:

```R
c5 <- get_complex(sim, sim_uid=5)
summary(c5$model) %>% print()
#> Call:
#> lm(formula = y ~ x, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.2936 -0.6818  0.0334  0.6788  3.0812 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.01987    0.06377   47.35   <2e-16 ***
#> x            1.99102    0.10875   18.31   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 1.009 on 998 degrees of freedom
#> Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2507 
#> F-statistic: 335.2 on 1 and 998 DF,  p-value: < 2.2e-16

c5$cov_mtx %>% print()
#>              (Intercept)           x
#> (Intercept)  0.004067129 -0.00600565
#> x           -0.006005650  0.01182677
```

## Using the `batch` function

The `batch` function is useful if you want to share data or objects between simulation replicates. Essentially, it allows you to take your simulation replicates and divide them into "batches"; all replicates in a given batch will then share a single set of objects. The most common use case for this is if you have a simulation that involves generating one dataset, analyzing it using multiple methods, and then repeating this a number of times. To illustrate the use of `batch` using this example, first consider the following simulation:

```R
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
#>   |########################################| 100%
#> Done. No errors or warnings detected.
```

If we look at the "dat_1" column of the results object (equal to the first element of the `dat` vector created in the simulation script), we see that a unique dataset was created for each simulation replicate:

```R
sim$results[order(sim$results$rep_id),]
#>   sim_uid level_id rep_id        est      runtime mu_hat dat_1
#> 1       1        1      1   est_mean 0.0006749630   3.03  4.67
#> 4       2        2      1 est_median 0.0071251392   3.12  3.69
#> 2       3        1      2   est_mean 0.0010349751   3.01  4.13
#> 5       5        2      2 est_median 0.0009829998   2.90  4.55
#> 3       4        1      3   est_mean 0.0009119511   2.97  1.74
#> 6       6        2      3 est_median 0.0010011196   3.07  3.89
```

Suppose that instead, we want to run a simulation where we generate one dataset, analyzing it using multiple methods (in this case corresponding to "est_mean" and "est_median"), and then repeating this twice. We can do this using the `batch` function, as follows:

```R
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
#>   |########################################| 100%
#> Done. No errors or warnings detected.
```

In the code above, we changed two things. One, we added `batch_levels=NULL` to the `set_config` call; don't worry about this for now. Second, we wrapped the code line `dat <- create_data(n=100)` inside the `batch` function. Whatever code goes inside the `batch` function will produce the same output for all simulations in a batch; in this case, if we look at the "dat_1" column of the results object, we can see that one dataset was created and shared by the batch corresponding to sim_uids 1 and 2:

```R
sim$results[order(sim$results$rep_id),]
#>   sim_uid level_id rep_id        est      runtime mu_hat dat_1
#> 1       1        1      1   est_mean 0.0009911060   2.99  3.93
#> 4       2        2      1 est_median 0.0049610138   3.04  3.93
#> 2       3        1      2   est_mean 0.0022661686   3.09  4.88
#> 5       5        2      2 est_median 0.0011191368   2.97  4.88
#> 3       4        1      3   est_mean 0.0008540154   2.94  3.20
#> 6       6        2      3 est_median 0.0008299351   2.96  3.20

```

However, the situation is often more complicated. What if we have a simulation with multiple level variables, some that correspond to creating data and some that correspond to analyzing the data? This is where the `batch_levels` config option comes in. It is easy to use; simply specify the names of the level variables that are used *within* the batch function. Here is an example:

```R
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
#>   |########################################| 100%
#> Done. No errors or warnings detected.

sim$results[order(sim$results$batch_id),]
#>    sim_uid level_id rep_id batch_id   n mu        est      runtime mu_hat dat_1
#> 1        1        1      1        1  10  3   est_mean 0.0009720325   2.84  3.26
#> 9        5        5      1        1  10  3 est_median 0.0017399788   2.64  3.26
#> 2        9        1      2        2  10  3   est_mean 0.0010929108   2.62  2.54
#> 10      13        5      2        2  10  3 est_median 0.0012290478   2.87  2.54
#> 3        2        2      1        3 100  3   est_mean 0.0121760368   3.03  4.01
#> 11       6        6      1        3 100  3 est_median 0.0016720295   3.01  4.01
#> 4       10        2      2        4 100  3   est_mean 0.0014288425   2.89  2.12
#> 12      14        6      2        4 100  3 est_median 0.0012660027   2.89  2.12
#> 5        3        3      1        5  10  5   est_mean 0.0010859966   4.77  4.24
#> 13       7        7      1        5  10  5 est_median 0.0014128685   4.74  4.24
#> 6       11        3      2        6  10  5   est_mean 0.0012428761   4.89  5.03
#> 14      15        7      2        6  10  5 est_median 0.0013070107   4.68  5.03
#> 7        4        4      1        7 100  5   est_mean 0.0011181831   5.07  6.25
#> 15       8        8      1        7 100  5 est_median 0.0011219978   5.00  6.25
#> 8       12        4      2        8 100  5   est_mean 0.0012068748   4.93  5.32
#> 16      16        8      2        8 100  5 est_median 0.0019259453   4.98  5.32
```

We see that we have achieved what we wanted - the batches were created such that each batch contained two replicates, one for each estimator. We also specified the `return_batch_id=T` option in `set_config` so that the results object would return the `batch_id`. The `batch_id` is the variable that defines the batches; all simulations that share the same batch_id. It can be useful to use this option to ensure that you are using the `batch` function correctly.

Here are a few tips to keep in mind when using the batch function:

1. The code within the `batch` code block should *only* create objects; do not attempt to change or delete existing objects, as these changes may be ignored.
2. In the majority of cases, the `batch` function will be called just once, at the top of the simulation script. However, it can be used anywhere in the script and can be called multiple times. Never use `batch` outside of the simulation script.
3. Although we illustrated the use of the `batch` function to create a dataset to share between multiple replicates, it can be used for much more, such as taking a sample from an existing dataset, computing shared nuisance function estimators, performing computationally-intense tasks, and so on.
4. Currently, if your simulation script uses the `batch` function, you cannot update the simulation using the `update_sim` (or `update_sim_on_cluster`), function *unless* all you are doing is removing replicates. This may be changed in the future.

## Setting seeds

In statistical research, it is often desirable to have the ability to reproduce the exact results of a simulation. Since R code often involves stochastic (random) functions like `rnorm` or `sample` that return different values when called multiple times, reproducibility is not guaranteed. In a simple R script, calling the `set.seed` function at the top of your script ensures that the code that follows will produce the same results whenever the script is run. However, a more nuanced strategy is needed when running simulations. If we are running 100 replicates of the same simulation, we typically don't want each replicate to return identical results; rather, we would like for each replicate to be different from one another, but for *the entire set of replicates* to be the same when we run the entire simulation twice in a row. Luckily, **SimEngine** manages this process for you, even when simulations are being run in parallel. **SimEngine** uses a single "global seed" that changes the individual seeds for each simulation replicate; use `set_config` to set or change this global seed:

```R
sim %<>% set_config(seed=123)
```

If you did not set a seed with `set_config`, **SimEngine** will set a random seed automatically for you so that you can reproduce the results later if desired. To view this seed, use the `vars` function:

```R
sim <- new_sim()
vars(sim, "seed") %>% print()
#> 561011367
```

## Using simulation constants

A "simulation constant"" is any R object that does not change across simulation replicates. Although constants can simply be declared as variables in the global namespace, it can be useful to "hack" the levels functionality of SimEngine to serve as an organizational container for global values that you may want to change later. The code below demonstrates this, where `beta1` and `beta2` are constants.

```R
sim <- new_sim()
my_data <- read.csv("my_data.csv")
sim %<>% set_levels(
  n = c(100,1000),
  beta0 = 3,
  beta1 = 1
)
create_data <- function() {
  x <- runif(L$n)
  y <- L$beta0 + L$beta1*x + rnorm(L$n)
  return(data.frame(x,y))
}
# ... and so on ...
```

## Handling errors and warnings

As with any type of programming, debugging is a necessary part of the coding workflow. With simulations, sometimes errors occur that will affect all simulation replicates and sometimes errors occur that only affecet some replicates. By default, when a simulation is run, **SimEngine** will not stop if an error occurs; instead, errors are logged and stored in a dataframe along with information about the simulation replicates that resulted in those errors. Examining this dataframe by typing `print(sim$errors)` can sometimes help to quickly pinpoint the issue. This is demonstrated below:

```R
sim <- new_sim()
sim %<>% set_config(num_sim=2)
sim %<>% set_levels(
  Sigma = list(
    s1 = matrix(c(3,1,1,2), nrow=2),
    s3 = matrix(c(4,3,3,9), nrow=2),
    s2 = matrix(c(1,2,2,1), nrow=2),
    s4 = matrix(c(8,2,2,6), nrow=2)
  )
)
sim %<>% set_script(function() {
  x <- MASS::mvrnorm(n=1, mu=c(0,0), Sigma=L$Sigma)
  return(list(x1=x[1], x2=x[2]))
})

sim %<>% run()
#> |########################################| 100%
#> Done. Errors detected in 25% of simulation replicates. Warnings detected in 0% of simulation replicates.

print(sim$errors)
#>   sim_uid level_id rep_id Sigma runtime                          message                                                call
#> 1       5        3      1    s2       0 'Sigma' is not positive definite MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma)
#> 2       6        3      2    s2       0 'Sigma' is not positive definite MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma)
```

From the output above, we see that our code fails for the simulation replicates that use the level with `Sigma="s2"` because it uses an invalid covariance matrix. Similarly, if a simulation involves replicates that throw warnings, all warnings are logged and stored in the dataframe `sim$warnings`.

The workflow above can be useful to quickly spot errors, but it has two main drawbacks. First, it can be frustrating to run a time-consuming simulation involving hundreds or thousands of replicates, only to find out at the very end that every replicate failed because of a typo. It is often useful to stop an entire simulation after a single error has occurred. Second, it can sometimes be difficult to determine exactly what caused an error without making use of more advanced debugging tools. For both of these situations, use the following configuration option:

```R
sim %<>% set_config(stop_at_error=TRUE)
```

Setting `stop_at_error=TRUE` will stop the simulation when it encounters any error. Furthermore, the error will be thrown by R in the usual way, and so if you are running the simulation in RStudio, you can make use of the built-in debugging tools to find and fix the bug. For example, you can click "Show Traceback" to view the entire call stack and see the function calls that led to the error, or you can click "Rerun with debug" to active the interactive debugger, which reruns the code and pauses execution where the error occurred so that you can examine objects in the function's environment. Try running the code above in RStudio but with the `stop_at_error=TRUE` configuration option, clicking "Rerun with debug", and then typing `print(Sigma)` in the console.
