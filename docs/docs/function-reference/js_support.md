---
layout: page
title: js_support 
nav_order: 3 
permalink: /function-reference/js_support/
parent: Function reference
---

<script type="text/javascript">
const macros = { "\\R": "\\textsf{R}", "\\code": "\\texttt"};
function processMathHTML() {
    var l = document.getElementsByClassName('reqn');
    for (let e of l) { katex.render(e.textContent, e, { throwOnError: false, macros }); }
    return;
}</script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.3/dist/katex.min.js"
    onload="processMathHTML();"></script>
<link rel="stylesheet" type="text/css" href="R.css" />
</head><body><div class="container">

<table style="width: 100%;"><tr><td>js_support {SimEngine}</td><td style="text-align: right;">R Documentation</td></tr></table>

<h2>Display information about currently-supported job schedulers</h2>

<h3>Description</h3>

<p>Run this function to display information about job schedulers
that are currently supported for running <span class="pkg">SimEngine</span> simulations on a
cluster computing system (CCS).
</p>


<h3>Usage</h3>

```R<code class='language-R'>js_support()
</span>```


<h3>Examples</h3>

```R<code class='language-R'>js_support()
</span>```

<hr /><div style="text-align: center;">[Package <em>SimEngine</em> version 1.1.0 ]</div>
</div>
