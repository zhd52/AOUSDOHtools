
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AOUSDOHtools

<!-- badges: start -->
<!-- badges: end -->

## Overview

`AOUSDOHtools` is an R package designed to process and analyze **Social
Determinants of Health (SDOH)** Survey data from the ***All of Us
(AOU)*** Research Program (<https://www.researchallofus.org/>). It
provides tools to streamline common survey data tasks, including
recoding, score calculation, and variable creation. The package is ideal
for researchers and analysts working with SDOH survey data within the
All of Us Research Hub Researcher Workbench, a cloud-based platform
supporting AOU data analysis
(<https://www.researchallofus.org/data-tools/survey-explorer/>).

This package is developed in conjunction with a published user guide:

- Theresa A Koleck, Caitlin Dreisbach, Chen Zhang, Susan Grayson,
  Maichou Lor, Zhirui Deng, Alex Conway, Peter D R Higgins, Suzanne
  Bakken, *User guide for Social Determinants of Health Survey data in
  the All of Us Research Program*, Journal of the American Medical
  Informatics Association, Volume 31, Issue 12, December 2024, Pages
  3032–3041, <https://doi.org/10.1093/jamia/ocae214>.

The package enables users to calculate health and well-being scores for
14 key social determinants of health constructs assessed in the *All of
Us* Social Determinants of Health Survey (AOUSDOH).

**Supported Constructs and Functions**

1.  **Neighborhood Cohesion** - `calc_cohesion`

2.  **Neighborhood Disorder** - `calc_disorder`,
    `calc_physical_disorder`, `calc_social_disorder`

3.  **Neighborhood Environment** - `calc_density`, `calc_spa`,
    `calc_crime_safety`, `calc_nei`

4.  **Social Support** - `calc_social_support`, `calc_ins_support`,
    `calc_emo_support`

5.  **Loneliness** - `calc_loneliness`

6.  **Perceived Everyday Discrimination** - `calc_edd_situation`,
    `calc_edd_frequency`, `calc_edd_chronicity`

7.  **Perceived Discrimination in Health Care Settings** -
    `calc_hcd_ever`, `calc_hcd_count`, `calc_hcd_sum`, `calc_hcd_mean`

8.  **Food Insecurity** - `calc_food_insecurity`

9.  **Housing Insecurity / Instability** - `calc_housing_insecurity`,
    `calc_num_moves`

10. **Housing Quality** - `calc_housing_quality`

11. **Perceived Stress** - `calc_stress_sum`, `calc_stress_category`

12. **Daily Spiritual Experiences** - `calc_spirit`

13. **Religious Service Attendance** - `calc_religious_attendance`

14. **English Proficiency** - `calc_other_language`,
    `calc_english_level`, `calc_english_proficient`

This package simplifies data processing for AOU researchers, ensuring
consistent, reproducible analyses of SDOH factors.

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

## Create a sample survey data frame
survey_df <- data.frame(
  person_id = c(1, 1, 1, 1, 2, 2, 2, 2),
  question_concept_id = c(40192463, 40192411, 40192499, 40192417,
                          40192463, 40192411, 40192499, 40192417),
  answer_concept_id = c(40192514, 40192455, 40192524, 40192408,
                        40192514, 40192455, 40192422, 40192408)
  )

## Compute neighborhood cohesion scores
cohesion_scores <- calc_cohesion(survey_df)
head(cohesion_scores)
#> # A tibble: 2 × 2
#>   person_id cohesion
#>       <dbl>    <dbl>
#> 1         1      3.5
#> 2         2      3
```
