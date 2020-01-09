[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/o6ad99s8n0q81gr2/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-resilience/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.resilience.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.resilience)
[![codecov](https://codecov.io/github/KWB-R/kwb.resilience/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.resilience)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.resilience)]()
[![DOI](https://zenodo.org/badge/138212756.svg)](https://zenodo.org/badge/latestdoi/138212756)

**R Package for the Quantification of Technical Resilience**

kwb.resilience allows quantification of a number of resilience indicators. 
Calculation requires a time series of performance values of a technical system, 
as well as values for acceptable and worst case performance.


## Installation

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.resilience' from GitHub

remotes::install_github("KWB-R/kwb.resilience")
```
