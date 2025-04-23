# smsets

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Description

The goal of `smsets` is to produce simple multivariate statistical tests and parameter estimation for means and variances / covariances when one single factor with two or more levels are involved. Analyses include:

* Multiple two-sample t-tests for means.
* Multiple two-sample Levene's tests for variances.
* Two-sample Hotelling's $T^2$ test.
* Extended Levene's tests for two multivariate samples based on Hotelling's $T^2$ test.
* Two-sample van Valen's test.
* One-way MANOVA.
* Box's M test of variation for multivariate samples (_F_ approximation).
* Penrose's distance calculator

## Getting started

You can install the development version of `smsets` from [GitHub](https://github.com) with:

```{r}
devtools::install_github("ganava4/smsets")
```

Afterwards, just load it and it will be ready to use.


```{r}
library("smsets")
```

