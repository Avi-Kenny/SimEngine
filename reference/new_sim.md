# Create a new simulation object

Create a new simulation object. This is typically the first function to
be called when running a simulation using SimEngine. Most other
SimEngine functions take a simulation object as their first argument.

## Usage

``` r
new_sim()
```

## Value

A simulation object, of class `sim_obj`

## See also

Visit <https://avi-kenny.github.io/SimEngine/> for more information on
how to use the SimEngine simulation framework.

## Examples

``` r
sim <- new_sim()
print(sim)
```
