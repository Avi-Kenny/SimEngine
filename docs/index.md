---
layout: default
title: Home
nav_order: 1
description: "simba: A simple but powerful simulation framework"
permalink: /
last_modified_date: 2020-04-27T17:54:08+0000
---

# simba
{: .fs-9 }

A simple but powerful simulation framework
{: .fs-6 .fw-300 }

[View source code on GitHub](https://github.com/Avi-Kenny/simba){: .btn .btn-primary .fs-5 .mb-4 .mb-md-0 .mr-2 }

---

## Overview

simba is an R package for structuring, maintaining, running, and debugging statistical simulations. [...]

Here is a simple example that illustrates the basic workflow:

1. Step one
```R
sim %<>% add_method(
  "sample_mean",
  function(x) {
    return ( mean(x) )
  }
)
```

2. Step two
```R
sim %<>% add_method(
  "sample_mean",
  function(x) {
    return ( mean(x) )
  }
)
```

3. Step three
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

simba is &copy; {{ "now" | date: "%Y" }} by [Avi Kenny](https://github.com/Avi-Kenny).

### License

simba is distributed by an [MIT license](!!!!! link to license on github).

### Contributing

When contributing to this repository, please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository before making a change. Read more about becoming a contributor in [our GitHub repo](https://github.com/pmarsceill/just-the-docs#contributing).

#### Thank you to the contributors of Just the Docs!

<ul class="list-style-none">
{% for contributor in site.github.contributors %}
  <li class="d-inline-block mr-1">
     <a href="{{ contributor.html_url }}"><img src="{{ contributor.avatar_url }}" width="32" height="32" alt="{{ contributor.login }}"/></a>
  </li>
{% endfor %}
</ul>

### Code of Conduct

Just the Docs is committed to fostering a welcoming community.

[View our Code of Conduct](https://github.com/pmarsceill/just-the-docs/tree/master/CODE_OF_CONDUCT.md) on our GitHub repository.
