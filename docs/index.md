---
layout: default
title: Home
nav_order: 1
description: "simba: A simple but powerful simulation framework"
permalink: /
last_modified_date: 2020-04-27T17:54:08+0000
---

<img src="assets/images/logo.png" align="right" alt="simba" style="height:150px">

# simba
{: .fs-9 }

A simple but powerful simulation framework
{: .fs-6 .fw-300 }

[View source code on GitHub](https://github.com/Avi-Kenny/simba){: .btn .btn-primary .fs-5 .mb-4 .mb-md-0 .mr-2 }

---

## Overview

**simba** is an R package for structuring, maintaining, running, and debugging statistical simulations. [...]

## Installation

**simba** is currently hosted on GitHub and can be easily installed using the **devtools** package:

```R
library(devtools)
install_github(repo="Avi-Kenny/simba")
```

## Getting started

Here is a simple example that illustrates the basic workflow.

1) Load the package and declare a new simulation object

```R
library(simba)
sim <- new_sim()
```

2) Set the simulation configuration

```R
sim %<>% set_config(
  num_sim = 5,
  parallel = "none",
  packages = c("magrittr", "dplyr")
)
```

3) Step three

```R
sim %<>% add_method(
  "sample_mean",
  function(x) {
    return ( mean(x) )
  }
)
```

---

## About the project

simba was created and is maintained by [Avi Kenny](https://github.com/Avi-Kenny). The package is licensed using the [GNU General Public Licence (GPL) V3](https://github.com/Avi-Kenny/simba/blob/master/LICENSE.txt).
