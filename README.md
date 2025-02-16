
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AOUSDOHtools

<!-- badges: start -->
<!-- badges: end -->

## Overview

`AOUSDOHtools` is an R package designed for processing and analyzing
survey data from the **All of Us Social Determinants of Health
(AOUSDOH)** program. The package provides tools to calculate various
health and well-being scores, such as: - Neighborhood cohesion - Social
support - Perceived discrimination - Perceived stress - Other
survey-based metrics

It simplifies common tasks in survey data analysis, including: -
Recoding - Score calculation - Variable creation

This package is ideal for researchers and analysts working with social
determinants of health (SDOH) survey data in the All of Us program.

## Installation

You can install the development version of `AOUSDOHtools` from GitHub
using the following steps:

``` r
# Install devtools if necessary
install.packages("devtools")

# Install AOUSDOHtools from GitHub
devtools::install_github("zhd52/AOUSDOHtools")
```

## Example

Once installed, you can use the package by loading it into your R
session:

``` r
library(AOUSDOHtools)

# Example: Calculating Neighborhood Cohesion Score

## Assuming `survey_df` is your survey data frame:
## cohesion_scores <- calc_cohesion(survey_df)
## head(cohesion_scores)
```
