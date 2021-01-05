---
layout: page
title: add_creator 
nav_order: 2 
permalink: /add_creator/
parent: Function reference
---


<table width="100%" summary="page for add_creator {simba}"><tr><td>add_creator {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Add a &quot;creator&quot; function</h2>

<h3>Description</h3>

<p>Add a &quot;creator&quot; function to your simulation object. A creator is
a function that generates a dataset for use in your simulation.
</p>


<h3>Usage</h3>

```R
add_creator(sim_obj, name, fn)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span></td>
<td>
<p>A name for the creator function</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span></td>
<td>
<p>A creator function</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>There are two ways to use <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>add_creator</span>. If two arguments are
supplied (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span> and <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span>), you can create a function
separately and add it to your simulation object later. If three arguments
are supplied, you can do both at the same time, using an anonymous
function for the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span> argument. See examples.
</p>
</li>
<li><p>Your creator will be stored in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj$creators</span>. If you added a
creator called <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>create_data</span>, you can test it out by running
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$creators$create_data()</span>. See examples.
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with the new creator function added
</p>


<h3>Examples</h3>

```R
# The first way to use add_creator is to declare a function and add it to
# your simulation object later:

sim <- new_sim()
create_data <- function (n) { rpois(n, lambda=5) }
sim %<>% add_creator(create_data)

# The second way is to do both at the same time:

sim <- new_sim()
sim %<>% add_creator("create_data", function(n) {
  rpois(n, lambda=5)
})

# With either option, you can test your function as follows:

sim$creators$create_data(10)
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
