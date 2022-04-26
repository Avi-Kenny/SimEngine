---
layout: page
title: new_sim 
nav_order: 3 
permalink: /function-reference/new_sim/
parent: Function reference
---


<table width="100%" summary="page for new_sim {SimEngine}"><tr><td>new_sim {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Create a new simulation object</h2>

<h3>Description</h3>

<p>Create a new simulation object. This is typically the first
function to be called when running a simulation using <span class="pkg">SimEngine</span>. Most
other <span class="pkg">SimEngine</span> functions take a simulation object as their first
argument.
</p>


<h3>Usage</h3>

```R
new_sim()
```


<h3>Value</h3>

<p>A simulation object, of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>
</p>


<h3>See Also</h3>

<p>Visit <a href="https://avi-kenny.github.io/SimEngine/">https://avi-kenny.github.io/SimEngine/</a> for more information on how to
use the <span class="pkg">SimEngine</span> simulation framework.
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.1.0 ]</div>
