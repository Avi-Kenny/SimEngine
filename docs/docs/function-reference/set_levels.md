---
layout: page
title: set_levels 
nav_order: 10 
permalink: /function-reference/set_levels/
parent: Function reference
---


<table width="100%" summary="page for set_levels {SimEngine}"><tr><td>set_levels {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Set simulation levels</h2>

<h3>Description</h3>

<p>Set one or more simulation levels, which are things that vary
between simulation replicates.
</p>


<h3>Usage</h3>

```R
set_levels(sim, ..., .add = FALSE, .keep = NA)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>new_sim</span></p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>...</span></td>
<td>
<p>One or more key-value pairs representing simulation levels. Each
value can either be a vector (for simple levels) or a list of lists (for
more complex levels). See examples.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>.add</span></td>
<td>
<p>Only relevant if <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_levels</span> is called twice or
more. On the second call, if add=FALSE (default) the old set of levels
will be replaced by the new set, whereas if add=TRUE the new set of
levels will be merged with the old set. See examples.</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>.keep</span></td>
<td>
<p>An integer vector specifying which level combinations to keep;
see examples.</p>
</td></tr>
</table>


<h3>Value</h3>

<p>The original simulation object with the old set of levels replaced
with the new set
</p>


<h3>Examples</h3>

```R
# Basic usage is as follows:
sim <- new_sim()
sim %<>% set_levels(
  "n" = c(10, 100, 1000),
  "theta" = c(2, 3)
)
sim$levels

# More complex levels can be set using lists:
sim %<>% set_levels(
  "n" = c(10, 100, 1000),
  "theta" = c(2, 3),
  "method" = list(
    "spline1" = list(knots=c(2,4), slopes=c(0.1,0.4)),
    "spline2" = list(knots=c(1,5), slopes=c(0.2,0.3))
  )
)
sim$levels

# By default, set_levels will overwrite old levels if it is called twice:
sim %<>% set_levels(alpha=c(1,2), beta=c(5,6))
sim %<>% set_levels(alpha=c(3,4), gamma=c(7,8))
sim$levels

# To merge the old levels with the new levels instead, specify .add=TRUE:
sim %<>% set_levels(alpha=c(1,2), beta=c(5,6))
sim %<>% set_levels(alpha=c(3,4), gamma=c(7,8), .add=TRUE)
sim$levels

# If you don't want to run simulations for all level combinations, use the
.keep option. First, set the levels normally. Second, view the
sim$levels_grid dataframe to examine the level combinations and the
associated level_id values. Third, call set_levels again with the .keep
option to specify which levels to keep (via a vector of level_id values).
sim %<>% set_levels(alpha=c(1,2,3), beta=c(5,6))
sim$levels_grid
sim %<>% set_levels(.keep=c(1,2,6))
sim$levels_grid
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
