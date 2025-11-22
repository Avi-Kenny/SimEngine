# Introduction to SimEngine

## Overview

**SimEngine** is an open-source R package for structuring, maintaining,
running, and debugging statistical simulations on both local and
cluster-based computing environments.

## Getting started

The goal of many statistical simulations is to compare the behavior of
two or more statistical methods; we use this framework to demonstrate
the **SimEngine** workflow. Most statistical simulations of this type
include three basic phases: (1) generate data, (2) run one or more
methods using the generated data, and (3) compare the performance of the
methods.

To briefly illustrate how these phases are implemented using , we use a
simple example of estimating the rate parameter $\lambda$ of a
$\text{Poisson}(\lambda)$ distribution. To anchor the simulation in a
real-world situation, one can imagine that a sample of size $n$ from
this Poisson distribution models the number of patients admitted daily
to a hospital over the course of $n$ consecutive days. Suppose that the
data consist of $n$ independent and identically distributed observations
$X_{1},X_{2},\ldots,X_{n}$ drawn from a Poisson($\lambda$) distribution.
Since the $\lambda$ parameter of the Poisson distribution is equal to
both the mean and the variance, one may ask whether the sample mean
(denoted ${\widehat{\lambda}}_{M,n}$) or the sample variance (denoted
${\widehat{\lambda}}_{V,n}$) is a better estimator of $\lambda$.

### 1) Load the package and create a simulation object

After loading the package, the first step is to create a simulation
object (an R object of class *sim_obj*) using the
[`new_sim()`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)
function. The simulation object contains all data, functions, and
results related to the simulation.

``` r
library(SimEngine)
#> Loading required package: magrittr
#> Welcome to SimEngine! Full package documentation can be found at:
#>  https://avi-kenny.github.io/SimEngine
sim <- new_sim()
```

### 2) Code a function to generate data

Many simulations involve a function that creates a dataset designed to
mimic a real-world data-generating mechanism. Here, we write and test a
simple function to generate a sample of `n` observations from a Poisson
distribution with $\lambda = 20$.

``` r
create_data <- function(n) {
  return(rpois(n=n, lambda=20))
}

create_data(n=10)
#>  [1] 24 15 19 22 25 11 16 18 17 16
```

### 3) Code the methods (or other functions)

With **SimEngine**, any functions declared (or loaded via
[`source()`](https://rdrr.io/r/base/source.html)) are automatically
stored in the simulation object when the simulation runs. In this
example, we test the sample mean and sample variance estimators of the
$\lambda$ parameter. For simplicity, we write this as a single function
and use the `type` argument to specify which estimator to use.

``` r
est_lambda <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
dat <- create_data(n=1000)
est_lambda(dat=dat, type="M")
#> [1] 19.952
est_lambda(dat=dat, type="V")
#> [1] 20.02172
```

### 4) Set the simulation levels

Often, we wish to run the same simulation multiple times. We refer to
each run as a *simulation replicate*. We may wish to vary certain
features of the simulation between replicates. In this example, perhaps
we choose to vary the sample size and the estimator used to estimate
$\lambda$. We refer to the features that vary as *simulation levels*; in
the example below, the simulation levels are the sample size (`n`) and
the estimator (`estimator`). We refer to the values that each simulation
level can take on as *level values*; in the example below, the `n` level
values are `10`, `100`, and `1000`, and the `estimator` level values are
`"M"` (for “sample mean”) and `"V"` (for “sample variance”). We also
refer to a combination of level values as a *scenario*; in this example,
the combination of `n=10` and `estimator="M"` is one of the six possible
scenarios defined by the two values of `n` and the three values of
`estimator`. By default, **SimEngine** runs one simulation replicate for
each scenario, although the user will typically want to increase this;
1,000 or 10,000 replicates per scenario is common.

``` r
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000)
)
```

Note that we make extensive use of the pipe operators (`%>%` and `%<>%`)
from the **magrittr** package; if you have never used pipes, see the
[magrittr documentation](https://magrittr.tidyverse.org).

### 5) Create a simulation script

The simulation script is a user-written function that assembles the
pieces above (generating data, analyzing the data, and returning
results) to code the flow of a single simulation replicate. Within a
script, the level values for the current scenario can be referenced
using the special variable `L`. For instance, in the running example,
when the first simulation replicate is running, `L$estimator` will equal
`"M"` and `L$n` will equal `10`. In the next replicate, `L$estimator`
will equal `"M"` and `L$n` will equal `100`, and so on. The simulation
script will automatically have access to any functions or objects that
have been declared in the global environment.

``` r
sim %<>% set_script(function() {
  dat <- create_data(n=L$n)
  lambda_hat <- est_lambda(dat=dat, type=L$estimator)
  return (list("lambda_hat"=lambda_hat))
})
```

The simulation script should always return a list containing one or more
key-value pairs, where the keys are syntactically valid names. The
values may be simple data types (numbers, character strings, or boolean
values) or more complex data types (lists, dataframes, model objects,
etc.); see the Advanced Usage documentation for how to handle complex
data types. Note that in this example, the estimators could have been
coded instead as two different functions and then called from within the
script using the
[`use_method()`](https://avi-kenny.github.io/SimEngine/reference/use_method.md)
function.

### 6) Set the simulation configuration

The
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
function controls options related to the entire simulation, such as the
number of simulation replicates to run for each scenario and the
parallelization type, if desired (see the Parallelization
documentation). Packages needed for the simulation should be specified
using the `packages` argument of
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
(rather than using [`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html)). We set `num_sim` to
100, and so **SimEngine** will run a total of 600 simulation replicates
(100 for each of the six scenarios).

``` r
sim %<>% set_config(
  num_sim = 100,
  packages = c("ggplot2", "stringr")
)
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:SimEngine':
#> 
#>     vars
```

### 7) Run the simulation

All 600 replicates are run at once and results are stored in the
simulation object.

``` r
sim %<>% run()
#> Done. No errors or warnings detected.
```

### 8) View and summarize results

Once the simulation replicates have finished running, the
[`summarize()`](https://avi-kenny.github.io/SimEngine/reference/summarize.md)
function can be used to calculate common summary statistics, such as
bias, variance, mean squared error (MSE), and confidence interval
coverage.

``` r
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)
#>   level_id estimator    n n_reps bias_lambda   mse_lambda
#> 1        1         M   10    100  -0.0170000   2.02670000
#> 2        2         V   10    100   2.0398889 147.39792963
#> 3        3         M  100    100   0.0358000   0.25405400
#> 4        4         V  100    100   0.2552697   7.69693198
#> 5        5         M 1000    100   0.0000200   0.01927814
#> 6        6         V 1000    100  -0.1457773   0.69827047
```

In this example, we see that the MSE of the sample variance is much
higher than that of the sample mean and that MSE decreases with
increasing sample size for both estimators, as expected. From the
`n_reps` column, we see that 100 replicates were successfully run for
each scenario. Results for individual simulation replicates can also be
directly accessed via the `sim$results` dataframe.

``` r
head(sim$results)
#>   sim_uid level_id rep_id estimator  n      runtime lambda_hat
#> 1       1        1      1         M 10 0.0004756451       21.2
#> 2       7        1      2         M 10 0.0002944469       19.8
#> 3       8        1      3         M 10 0.0807914734       19.0
#> 4       9        1      4         M 10 0.0004608631       22.3
#> 5      10        1      5         M 10 0.0003521442       21.2
#> 6      11        1      6         M 10 0.0003054142       20.0
```

Above, the `sim_uid` uniquely identifies a single simulation replicate
and the `level_id` uniquely identifies a scenario (i.e., a combination
of level values). The `rep_id` is unique within a given scenario and
identifies the index of that replicate within the scenario. The
`runtime` column shows the runtime of each replicate (in seconds).

### 9) Update a simulation

After running a simulation, a user may want to update it by adding
additional level values or replicates; this can be done with the
[`update_sim()`](https://avi-kenny.github.io/SimEngine/reference/update_sim.md)
function. Prior to running
[`update_sim()`](https://avi-kenny.github.io/SimEngine/reference/update_sim.md),
the functions
[`set_levels()`](https://avi-kenny.github.io/SimEngine/reference/set_levels.md)
and/or
[`set_config()`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
are used to declare the updates that should be performed. For example,
the following code sets the total number of replicates to 200 (i.e.,
adding 100 replicates to those that have already been run) for each
scenario, and adds one additional level value for `n`.

``` r
sim %<>% set_config(num_sim = 200)
sim %<>% set_levels(
  estimator = c("M", "V"),
  n = c(10, 100, 1000, 10000)
)
```

After the levels and/or configuration are updated,
[`update_sim()`](https://avi-kenny.github.io/SimEngine/reference/update_sim.md)
is called.

``` r
sim %<>% update_sim()
#> Done. No errors or warnings detected.
```

Another call to
[`summarize()`](https://avi-kenny.github.io/SimEngine/reference/summarize.md)
shows that the additional replicates were successfully:

``` r
sim %>% summarize(
  list(stat="bias", name="bias_lambda", estimate="lambda_hat", truth=20),
  list(stat="mse", name="mse_lambda", estimate="lambda_hat", truth=20)
)
#>   level_id estimator     n n_reps bias_lambda   mse_lambda
#> 1        1         M    10    200 -0.01500000   1.92640000
#> 2        2         V    10    200  0.75911111 118.55581235
#> 3        3         M   100    200  0.02630000   0.22649900
#> 4        4         V   100    200 -0.04132424   6.64234842
#> 5        5         M  1000    200 -0.00612000   0.02069753
#> 6        6         V  1000    200 -0.07187527   0.66624237
#> 7        7         M 10000    200  0.00080950   0.00198466
#> 8        8         V 10000    200 -0.01427990   0.08795799
```
