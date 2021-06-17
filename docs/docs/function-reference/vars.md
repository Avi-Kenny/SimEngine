---
layout: page
title: vars 
nav_order: 14 
permalink: /function-reference/vars/
parent: Function reference
---


<table width="100%" summary="page for vars {simba}"><tr><td>vars {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Access internal simulation variables</h2>

<h3>Description</h3>

<p>This is a &quot;getter function&quot; that returns the value of an
internal simulation variable.
</p>


<h3>Usage</h3>

```R
vars(sim_obj, var)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>var</span></td>
<td>
<p>If this argument is omitted, <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>vars()</span> will return a list
containing all available internal variables. If this argument is
provided, it should equal one of the following character strings:
</p>

<ul>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>config</span>: a list representing the current simulation
configuration. This will typically be set using <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>set_config()</span>.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>env</span>: a reference to the environment in which individual
simulation replicates are run (advanced)
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>num_sim_total</span>: The total number of simulation replicates
for the simulation. This is particularly useful when a simulation is
being run in parallel on a cluster computing system as a job array and
the user needs to know the range of task IDs.
</p>
</li>
<li><p><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>run_state</span>: A character string describing the &quot;run state&quot; of
the simulation. This will equal one of the following: &quot;pre run&quot; (the
simulation has not yet been run), &quot;run, no errors&quot; (the simulation ran
and had no errors), &quot;run, some errors&quot; (the simulation ran and had some
errors), &quot;run, all errors&quot; (the simulation ran and all replicates had
errors).
</p>
</li></ul>
</td></tr>
</table>


<h3>Value</h3>

<p>The value of the internal variable.
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% set_levels(
  "n" = c(10, 100, 1000)
)
sim %<>% set_config(num_sim=10)
value <- vars(sim, "num_sim_total")
value
all_values <- vars(sim)
all_values
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
