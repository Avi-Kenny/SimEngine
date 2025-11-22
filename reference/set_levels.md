# Set simulation levels

Set one or more simulation levels, which are things that vary between
simulation replicates.

## Usage

``` r
set_levels(sim, ..., .keep = NA)
```

## Arguments

- sim:

  A simulation object of class `sim_obj`, usually created by
  [`new_sim`](https://avi-kenny.github.io/SimEngine/reference/new_sim.md)

- ...:

  One or more key-value pairs representing simulation levels. Each value
  can either be a vector (for simple levels) or a list of lists (for
  more complex levels). See examples.

- .keep:

  An integer vector of level_id values specifying which scenarios to
  keep; see the Advanced Functionality documentation.

## Value

The original simulation object with the old set of levels replaced with
the new set

## Examples

``` r
# Basic simulation levels are numeric or character vectors
sim <- new_sim()
sim %<>% set_levels(
  n = c(10, 100, 1000),
  est = c("M", "V")
)

# Complex simulation levels can be set using named lists of lists
sim <- new_sim()
sim %<>% set_levels(
  n = c(10, 100, 1000),
  distribution = list(
    "Beta 1" = list(type="Beta", params=c(0.3, 0.7)),
    "Beta 2" = list(type="Beta", params=c(1.5, 0.4)),
    "Normal" = list(type="Normal", params=c(3.0, 0.2))
  )
)
```
