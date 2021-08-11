---
layout: page
title: add_constants 
nav_order: 1 
permalink: /function-reference/add_constants/
parent: Function reference
---


<table width="100%" summary="page for add_constants {simba}"><tr><td>add_constants {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Add one or more simulation constants</h2>

<h3>Description</h3>

<p>Add one or more simulation constants
</p>


<h3>Usage</h3>

```R
add_constants(sim_obj, ...)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class &quot;simba&quot;, usually created by
new_sim()</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>...</span></td>
<td>
<p>Key-value pairs will be added as &quot;simulation constants&quot; (i.e.
objects that don't change across simulations). Keys should be strings.
The purpose of this (rather than &quot;hard-coding&quot; constants in your scripts)
is to serve as an organizational container to easily change constants
later, and so that constants are automatically available on each cluster
node if you decide to run your simulation code in parallel</p>
</td></tr>
</table>


<h3>Value</h3>

<p>The original simulation object with added constants
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% add_constants(alpha=4, beta=c(1,2,3))
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 1.0.0 ]</div>
