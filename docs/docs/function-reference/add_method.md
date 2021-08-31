---
layout: page
title: add_method 
nav_order: 3 
permalink: /function-reference/add_method/
parent: Function reference
---


<table width="100%" summary="page for add_method {SimEngine}"><tr><td>add_method {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Add a &quot;method&quot; function</h2>

<h3>Description</h3>

<p>Add a &quot;method&quot; function to your simulation object. A method
function is just a function, and can be used anywhere that you would
normally write and use a regular global function. The advantages of
explicitly adding a method function to your simulation (rather than
declaring and using a function within your simulation script) are that
(1) you can use the method function as a simulation level, and (2)
parallelization is automated. Often, the method function will be a
statistical method that you want to test (e.g. an estimator), and will
take in a dataset returned by a creator function as its first argument;
however, this is not always the case.
</p>


<h3>Usage</h3>

```R
add_method(sim, name, fn)
```


<h3>Arguments</h3>

<table summary="R argblock">
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span></td>
<td>
<p>A simulation object of class <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim_obj</span>, usually created by
new_sim</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>name</span></td>
<td>
<p>A name for the method function</p>
</td></tr>
<tr valign="top"><td><span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span></td>
<td>
<p>A method function</p>
</td></tr>
</table>


<h3>Details</h3>


<ul>
<li><p>As with add_creator, there are two ways to use
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>add_method</span>. If two arguments are supplied (<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim</span> and
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span>), you can create a function separately and add it to your
simulation object later. If three arguments are supplied, you can do both
at the same time, using an anonymous function for the <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>fn</span> argument.
See examples.
</p>
</li>
<li><p>Your method will be stored in <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$methods</span>. If you added a
method called <span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>estimator_1</span>, you can test it out by running
<span style='font-family:&quot;SFMono-Regular&quot;,Menlo,Consolas,Monospace; font-size:0.85em'>sim$creators$estimator_1()</span>. See examples.
</p>
</li></ul>



<h3>Value</h3>

<p>The original simulation object with the new method function added
</p>


<h3>Examples</h3>

```R
sim <- new_sim()
sim %<>% add_creator("create_data", function(n) { rpois(n, lambda=5) })

# The first way to use add_method is to declare a function and add it to
# your simulation object later:

estimator_1 <- function (dat) { mean(dat) }
sim %<>% add_method(estimator_1)

# The second way is to do both at the same time:

sim %<>% add_method("estimator_2", function(dat) {
  var(dat)
})

# With either option, you can test your function as follows:

dat <- sim$creators$create_data(10)
sim$methods$estimator_1(dat)
sim$methods$estimator_2(dat)
```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.0.0 ]</div>
