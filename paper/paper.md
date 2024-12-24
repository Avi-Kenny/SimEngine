---
title: 'SimEngine: A Modular Framework for Statistical Simulations in R'
tags:
  - R
  - statistical simulations
  - cluster computing
  - parallel
authors:
  - name: Avi Kenny
    orcid: 0000-0002-9465-7307
    equal-contrib: true
    affiliation: "1, 2"
    corresponding: true
  - name: Charles J. Wolock
    orcid: 0000-0003-3527-1102
    equal-contrib: true
    affiliation: 3
  - name: Author Without ORCID
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 2
affiliations:
 - name: Department of Biostatistics & Bioinformatics, Duke University, Durham, NC
   index: 1
 - name: Global Health Institute, Duke University, Durham, NC
   index: 2
 - name: University of Pennsylvania
   index: 3
date: 24 December 2024
bibliography: paper.bib
---

# Summary

This article describes `SimEngine`, an open-source **R** package for structuring, maintaining, running, and debugging statistical simulations on both local and cluster-based computing environments. Several **R** packages exist for facilitating simulations, but `SimEngine` is the only package specifically designed for running simulations in parallel via job schedulers on high-performance cluster computing systems. The package provides structure and functionality for common simulation tasks, such as setting simulation levels, managing seeds for random number generation, and calculating summary metrics (such as bias and confidence interval coverage). `SimEngine` also brings several unique features, such as automatic calculation of Monte Carlo error and information-sharing across simulation replicates. We provide an overview of the package and demonstrate some of its advanced functionality.

# Statement of need

`Gala` is an Astropy-affiliated Python package for galactic dynamics. Python
enables wrapping low-level languages (e.g., C) for speed without losing
flexibility or ease-of-use in the user-interface. The API for `Gala` was
designed to provide a class-based and user-friendly interface to fast (C or
Cython-optimized) implementations of common operations such as gravitational
potential and force evaluation, orbit integration, dynamical transformations,
and chaos indicators for nonlinear dynamics. `Gala` also relies heavily on and
interfaces well with the implementations of physical units and astronomical
coordinate systems in the `Astropy` package [@astropy] (`astropy.units` and
`astropy.coordinates`).

`Gala` was designed to be used by both astronomical researchers and by
students in courses on gravitational dynamics or astronomy. It has already been
used in a number of scientific publications [@Pearson:2017] and has also been
used in graduate courses on Galactic dynamics to, e.g., provide interactive
visualizations of textbook material [@Binney:2008]. The combination of speed,
design, and support for Astropy functionality in `Gala` will enable exciting
scientific explorations of forthcoming data releases from the *Gaia* mission
[@gaia] by students and experts alike.

# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
