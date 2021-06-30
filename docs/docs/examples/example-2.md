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

In this example, we will consider the problem of estimating the variance-covariance matrix of the least-squares estimator in linear regression. We assume the reader has some familiarity with linear regression. 

Suppose our dataset consists of $$n$$ independent observations $$\{(Y_1, X_1), \dots, (Y_n, X_n)\}$$, where $$X$$ and $$Y$$ are both scalar variables. A general linear regression model posits that

$$ Y_i = \beta_0 + \beta_1 X_i + \epsilon_i$$,

where $$\epsilon_i$$ is a mean-zero noise term with variance $$\sigma^2_i$$. We refer to this as a heteroskedastic model, since the variances need not be equal across all $$i$$. This is the true data-generating model. (Note: A more restrictive (but misspecified) model assumes that there is a common variance $$\sigma^2$$ such that $$\sigma^2_i = \sigma^2$$ for all $i$. We refer to this incorrect model as the homoskedastic model.) 

For simplicity, we build a matrix $$\mathbb{X}$$, whose first column is all 1's (the intercept column) and whose second column is $$(X_1, \dots, X_n)^T$$. We also define a matrix 

$$ \Sigma = \begin{pmatrix}\sigma^2_1 &\dots& 0 \\ \vdots &\ddots& \vdots \\ 0 & \dots & \sigma^2_n\end{pmatrix}$$.

The ordinary least squares (OLS) estimator of $$\beta$$ is $$\hat{\beta} = (\mathbb{X}^T\mathbb{X})^{-1}\mathbb{X}^T\mathbb{Y}$$. The variance of this estimator, which we call $$\mathbb{V}$$ is

$$ \text{Var}(\hat{\beta}) = (\mathbb{X}^T\mathbb{X})^{-1}\mathbb{X}^T\Sigma\mathbb{X}(\mathbb{X}^T\mathbb{X})^{-1} $$.

The usual estimator of $$\mathbb{V}$$ is the model-based standard error $$s^2(\mathbb{X}^T\mathbb{X})^{-1}$$, where $$s^2 = \frac{\sum_i (Y_i - (\hat{\beta}_0 + \hat{\beta}_1X_i))^2}{n-1}$$. This is the estimator used by default in most statistical software (including the `lm()` function in R). However, under our heteroskedastic model where the $$\sigma^2_i$$ are not all equal, this is not a consistent estimator of $$\mathbb{V}$$! That is to say, even in large samples, we do not expect this estimator to be close to the truth.

A better estimator for our setting is the so-called sandwich standard error, or Huber-White standard error, given by

$$ (\mathbb{X}^T\mathbb{X})^{-1}\mathbb{X}^T\hat{\Sigma}\mathbb{X}(\mathbb{X}^T\mathbb{X})^{-1}$$,

where 

$$\hat{\Sigma} = \begin{pmatrix}(Y_1 - (\hat{\beta}_0 + \hat{\beta}_1X_1))^2  &\dots& 0 \\ \vdots &\ddots& \vdots \\ 0 & \dots & (Y_n - (\hat{\beta}_0 + \hat{\beta}_1X_n))^2\end{pmatrix}$$.

This estimator will be consistent under heteroskedasticity. 
