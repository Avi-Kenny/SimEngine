---
layout: page
title: new_sim 
nav_order: 5 
permalink: /function-reference/new_sim/
parent: Function reference
---


<table width="100%" summary="page for new_sim {simba}"><tr><td>new_sim {simba}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Create a new simulation object</h2>

<h3>Description</h3>

<p>Create a new simulation object. This is typically the first
function to be called when running a simulation using <span class="pkg">simba</span>. Most
other <span class="pkg">simba</span> functions take a simulation object as their first
argument.
</p>


<h3>Usage</h3>

```R
new_sim()
```


<h3>Value</h3>

<p>A simulation object, of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>
</p>


<h3>See Also</h3>

<p>Visit <a href="https://avi-kenny.github.io/simba">https://avi-kenny.github.io/simba</a> for more information on how to
use the <span class="pkg">simba</span> simulation framework.
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim
```

<hr /><div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>
