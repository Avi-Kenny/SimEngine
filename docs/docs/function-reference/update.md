---
layout: page
title: update 
nav_order: 11 
permalink: /update/
parent: Function reference
---

<table width="100%" summary="page for update {simba}"><tr>
<td>update {simba}</td>
<td style="text-align: right;">R Documentation</td>
</tr></table>
<h2>Update a simulation</h2><h3>Description</h3><p>!!!!! TO DO
</p><h3>Usage</h3>```R
update(sim_obj, keep_errors = TRUE, keep_extra = FALSE)
```<h3>Arguments</h3><table summary="R argblock">
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>simba</span>, usually created by
new_sim, that has already been run by the run function</p>
</td>
</tr>
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_errors</span></td>
<td>
<p>logical (TRUE by default); if TRUE, do not try to re-run
simulation reps that results in errors previously; if FALSE, attempt to
run those reps again</p>
</td>
</tr>
<tr valign="top">
<td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>keep_extra</span></td>
<td>
<p>logical (FALSE by default); if TRUE, keep previously run
simulation reps even if they exceed the current num_sim in config or are from
a level that has been dropped; if FALSE, drop excess reps (starting from the last rep
for that particular simulation level)</p>
</td>
</tr>
</table>
<h3>Examples</h3>```R
!!!!! TO DO
```<hr>
<div style="text-align: center;">[Package <em>simba</em> version 0.1.0.9000 ]</div>