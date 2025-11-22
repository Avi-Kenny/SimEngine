# Access internal simulation variables

This is a "getter function" that returns the value of an internal
simulation variable. Do not change any of these variables manually.

## Usage

``` r
vars(sim, var)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)

- var:

  If this argument is omitted, `vars` will return a list containing all
  available internal variables. If this argument is provided, it should
  equal one of the following character strings:

  - `seed`: the simulation seed; see
    [`set_config`](https://avi-kenny.github.io/SimEngine/reference/set_config.md)
    for more info on seeds.

  - `env`: a reference to the environment in which individual simulation
    replicates are run (advanced)

  - `num_sim_total`: The total number of simulation replicates for the
    simulation. This is particularly useful when a simulation is being
    run in parallel on a cluster computing system as a job array and the
    user needs to know the range of task IDs.

  - `run_state`: A character string describing the "run state" of the
    simulation. This will equal one of the following: "pre run" (the
    simulation has not yet been run), "run, no errors" (the simulation
    ran and had no errors), "run, some errors" (the simulation ran and
    had some errors), "run, all errors" (the simulation ran and all
    replicates had errors).

  - `session_info`: The results of a call to utils::sessionInfo() that
    occures when
    [new_sim](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)
    is called.

## Value

The value of the internal variable.

## Examples

``` r
sim <- new_sim()
sim %<>% set_levels(n = c(10, 100, 1000))
sim %<>% set_config(num_sim=10)
print(vars(sim, "seed"))
print(vars(sim, "env"))
print(vars(sim, "num_sim_total"))
```
