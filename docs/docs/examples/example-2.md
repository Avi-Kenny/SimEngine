---
layout: page
title: Comparing two standard error estimators
nav_order: 2
permalink: /examples/2
parent: Examples
usemathjax: true
---

# Example 2: Comparing two standard error estimators
{: .fs-9 }

---

When developing a novel statistical method, we often wish to compare our proposed method with one or more existing methods. This serves to highlight the differences between our method and whatever is used in common practice. Generally, we wish to examine realistic settings, motivated by statistical theory, in which the novel method confers some advantage over the alternatives. 

In this example, we will consider the problem of estimating the asymptotic (large-sample) variance-covariance matrix of the least-squares estimator in linear regression. We assume the reader has some familiarity with linear regression. 

Suppose our dataset consists of $$n$$ observations $$\{(Y_1, \bm{X}_1), \dots, (Y_n, \bm{X}_n)\}$$. 
