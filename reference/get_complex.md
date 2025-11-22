# Access internal simulation variables

Extract complex simulation data from a simulation object

## Usage

``` r
get_complex(sim, sim_uid)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)

- sim_uid:

  The unique identifier of a single simulation replicate. This
  corresponds to the `sim_uid` column in `sim$results`.

## Value

The value of the complex simulation result data corresponding to the
supplied `sim_uid`, as well as the `rep_id`, `level_id`, and associated
level variables.

## Examples

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
c5 <- get_complex(sim, sim_uid=5)
print(summary(c5$model))
print(c5$cov_mtx)
```
