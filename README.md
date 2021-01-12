---
output:
  md_document:
    variant: gfm
---



# pricesensitivitymeter

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/pricesensitivitymeter)](https://cran.r-project.org/package=pricesensitivitymeter)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/grand-total/pricesensitivitymeter)](https://cran.r-project.org/package=pricesensitivitymeter)
[![R-CMD-check](https://github.com/max-alletsee/pricesensitivitymeter/workflows/R-CMD-check/badge.svg)](https://github.com/max-alletsee/pricesensitivitymeter/actions)
[![codecov](https://codecov.io/gh/max-alletsee/pricesensitivitymeter/branch/master/graph/badge.svg?token=W1JHNAMMEB)](https://codecov.io/gh/max-alletsee/pricesensitivitymeter)

## Overview 
pricesensitivitymeter is an implementation of the van Westendorp Price Sensitivity Meter (PSM) in R, which is a popular method in market research to analyze consumer price preferences and price sensitivity. It also covers the so-called Newton Miller Smith Extension which allows to estimate prices that maximize the trial rate and the revenue.

## Installation

As of version 0.2.1, this package is [available on CRAN](https://cran.r-project.org/package=pricesensitivitymeter).


```r
# install the stable release from CRAN
install.packages("pricesensitivitymeter")

# install the development version from Github
devtools::install_github("max-alletsee/pricesensitivitymeter")
```

## Usage

The main function of the package is `psm_analysis()` which performs all necessary analyses. 


```r
## creating example data

tch <- round(rnorm(n = 250, mean = 5, sd = 0.5), digits = 2)
ch <- round(rnorm(n = 250, mean = 8.5, sd = 0.5), digits = 2)
ex <- round(rnorm(n = 250, mean = 13, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 17, sd = 1), digits = 2)

data_psm_demo <- data.frame(tch, ch, ex, tex)

## running the analysis
output_psm_demo <- psm_analysis(
  toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  data = data_psm_demo
)

summary(output_psm_demo)
```

The package also has a function `psm_analysis_weighted()` that deals with weighted survey data where the survey design for the weighting is coming from the `survey` package. (Please see the documentation and the vignette for more details.) Moreover, there is a convenience function `psm_plot()` that creates the layout for the standard Price Sensitivity Meter plot via `ggplot2`. Below is a simple example, using the analysis result created above.



