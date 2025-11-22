# Update a simulation

This function updates a previously run simulation. After a simulation
has been
[`run`](https://avi-kenny.github.io/SimEngine/reference/run.md), you can
alter the levels of the resulting object of class `sim_obj` using
[`set_levels`](https://avi-kenny.github.io/SimEngine/reference/set_levels.md),
or change the configuration (including the number of simulation
replicates) using
[`set_config`](https://avi-kenny.github.io/SimEngine/reference/set_config.md).
Executing `update_sim` on this simulation object will only run the added
levels/replicates, without repeating anything that has already been run.

## Usage

``` r
update_sim(sim, keep_errors = T)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md),
  that has already been run by the
  [`run`](https://avi-kenny.github.io/SimEngine/reference/run.md)
  function

- keep_errors:

  logical (`TRUE` by default); if `TRUE`, do not try to re-run
  simulation reps that results in errors previously; if `FALSE`, attempt
  to run those reps again

## Value

The original simulation object with additional simulation replicates in
`results` or `errors`

## Details

- It is not possible to add new level variables, only new levels of the
  existing variables. Because of this, it is best practice to include
  all potential level variables before initially running a simulation,
  even if some of them only contain a single level. This way, additional
  levels can be added later.

## Examples

``` r
sim <- new_sim()
create_data <- function(n) { rpois(n, lambda=5) }
est_mean <- function(dat, type) {
  if (type=="M") { return(mean(dat)) }
  if (type=="V") { return(var(dat)) }
}
sim %<>% set_levels(n=c(10,100), est="M")
sim %<>% set_config(num_sim=10)
sim %<>% set_script(function() {
  dat <- create_data(L$n)
  lambda_hat <- est_mean(dat=dat, type=L$est)
  return (list("lambda_hat"=lambda_hat))
})
sim %<>% run()
sim %>% summarize(list(stat="mean", x="lambda_hat"))
sim %<>% set_levels(n=c(10,100,1000), est=c("M","V"))
sim %<>% set_config(num_sim=5)
sim %<>% update_sim()
sim %>% summarize(list(stat="mean", x="lambda_hat"))
```
