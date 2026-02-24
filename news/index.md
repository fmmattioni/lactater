# Changelog

## lactater 0.3.0

- Improved
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  performance by reducing repeated model preparation and minimizing
  expensive data wrangling in method pipelines.
- Optimized plotting performance by removing
  [`ggplot_build()`](https://ggplot2.tidyverse.org/reference/ggplot_build.html)
  from x-axis adjustment and simplifying plot construction paths.
- Added stronger input/model error handling for edge cases in fitting
  and segmentation routines.
- Expanded test coverage with validation and golden-output regression
  tests for numerical stability.
- Updated CI quality checks and coverage gates to better protect
  performance and correctness changes.

## lactater 0.2.0

CRAN release: 2023-11-16

- Added the function
  [`lactate_curve()`](https://fmmattioni.github.io/lactater/reference/lactate_curve.md)
  to retrieve data for plotting the lactate curve and heart rate
  response.

## lactater 0.1.4

CRAN release: 2023-04-16

- Patched a modification to the `Dmax` method, which was producing
  negative values in a few edge cases.

- Make sure that infinite values are not retrieved when converting
  intensity to log.

## lactater 0.1.3

- Patched a fix to the `Dmax` method that was producing very high
  estimations in a few cases.

## lactater 0.1.2

CRAN release: 2023-02-22

- Fixed missing log in intensity in the `Log-log` method.

## lactater 0.1.1

CRAN release: 2022-09-08

- Removed `align` attribute in `img` tag.

## lactater 0.1.0

CRAN release: 2021-12-03

- Added a `NEWS.md` file to track changes to the package.
