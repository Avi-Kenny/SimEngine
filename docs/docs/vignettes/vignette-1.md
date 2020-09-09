---
layout: page
title: Vignette 1
permalink: /vignette1/
parent: Vignettes
---

![simba](/images/logo.png){:height="150px"}
<hr>

# Heading 1

<h2>{{ site.data.samplelist.docs_list_title }}</h2>
<ul>
   {% for item in site.data.samplelist.docs %}
      <li><a href="{{ item.url }}">{{ item.title }}</a></li>
   {% endfor %}
</ul>

## Heading 2

<ul>
    {% for item in site.data.samplelist[page.sidebar] %}
      <li><a href="{{ item.url }}">{{ item.title }}</a></li>
    {% endfor %}
</ul>

*Hey there*

Hey there

List
- one
- two
- three

List
1. one
2. two
3. three


This is a code block:
`<head>My header</head>`
