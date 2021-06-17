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

Often, simulation levels will be simple, such as a vector of sample sizes. However, !!!!!

```R
TO DO
```

## Setting seeds

When R code involves random functions like `rnorm()` or `sample()`, it is often desirable to have the ability to reproduce exact results. In a simple R script, calling the `set.seed()` function at the top of your script ensures that the code that follows will produce the same results whenever the script is run. However, a more nuanced strategy is needed when running simulations. If we are running 100 replicates of the same simulation, we typically don't want each replicate to return identical results; rather, we would like for each replicate to be different from one another, but for *the entire set of replicates* to be the same when we run the entire simulation twice in a row. Luckily, **simba** manages this process for you, even when simulations are being run in parallel. **simba** uses a single "global seed" that changes the individual seeds for each simulation replicate; if desired you can use `set_config()` to change this global seed:

```R
sim %<>% set_config(seed=123)
```

## Using simulation constants

A simulation constant is any R object that does not change across simulation replicates. It can be useful as an organizational "container" to store global values that you may want to change later. It can also be used to store external data that you need in your simulation. This is especially useful when the external data requires some amount of time-intensive processing that you only want to do once. The code below demonstrates both uses of simulation constants.

```R
sim <- new_sim()
my_data <- read.csv("my_data.csv")
sim %<>% add_constants(
  samp_size = 3,
  my_data = read.csv("my_data.csv")
)

sim %<>% set_script(function() {
  sample_indices <- sample(c(1:nrow(my_data)), size=C$samp_size)
  my_sample <- C$my_data[sample_indices,]
  estimate <- mean(my_sample$value)
  return (list("estimate" = estimate))
})

```

## Handling errors and warnings

As with any type of programming, debugging is a necessary part of the coding workflow. With simulations, sometimes errors occur that will affect all simulation replicates and sometimes errors occur that only affecet some replicates. By default, when a simulation is run, **simba** will not stop if an error occurs; instead, errors are logged and stored in a dataframe along with information about the simulation replicates that resulted in those errors. Examining this dataframe by typing `print(sim$errors)` can sometimes help to quickly pinpoint the issue. This is demonstrated below:

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
#>   sim_uid level_id sim_id Sigma runtime                          message                                                call
#> 1       5        3      1    s2       0 'Sigma' is not positive definite MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma)
#> 2       6        3      2    s2       0 'Sigma' is not positive definite MASS::mvrnorm(n = 1, mu = c(0, 0), Sigma = L$Sigma)
```

From the output above, we see that our code fails for the simulation replicates that use the level with `Sigma="s2"` because it uses an invalid covariance matrix. Similarly, if a simulation involves replicates that throw warnings, all warnings are logged and stored in the dataframe `sim$warnings`.

The workflow above can be useful to quickly spot errors, but it has two main drawbacks. First, it can be frustrating to run a time-consuming simulation involving hundreds or thousands of replicates, only to find out at the very end that every replicate failed because of a typo. It is often useful to stop an entire simulation after a single error has occurred. Second, it can sometimes be difficult to determine exactly what caused an error without making use of more advanced debugging tools. For both of these situations, use the following configuration option:

```R
sim %<>% set_config(stop_at_error=TRUE)
```

Setting `stop_at_error=TRUE` will stop the simulation when it encounters any error. Furthermore, the error will be thrown by R in the usual way, and so if you are running the simulation in RStudio, you can make use of the built-in debugging tools to find and fix the bug. For example, you can click "Show Traceback" to view the entire call stack and see the function calls that led to the error, or you can click "Rerun with debug" to active the interactive debugger, which reruns the code and pauses execution where the error occurred so that you can examine objects in the function's environment. Try running the code above in RStudio but with the `stop_at_error=TRUE` configuration option, clicking "Rerun with debug", and then typing `print(Sigma)` in the console.
