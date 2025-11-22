# Run the simulation

This is the workhorse function of SimEngine that actually runs the
simulation. This should be called after all functions that set up the
simulation (`set_config`, `set_script`, etc.) have been called.

## Usage

``` r
run(sim)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)

## Value

The original simulation object but with the results attached (along with
any errors and warnings). Results are stored in `sim$results`, errors
are stored in `sim$errors`, and warnings are stored in `sim$warnings`.

## Examples

``` r
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
sim %<>% set_config(num_sim=1)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat, type=L$est)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim$results %>% print()
```
