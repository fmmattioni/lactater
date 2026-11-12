# Lactate threshold report table

Formats the result of
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
as a publication-ready table: thresholds grouped by method family, with
the confidence interval when available, and the checks that need
attention as notes. Needs the gt package.

## Usage

``` r
lactate_report(x, title = "Lactate thresholds")
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md).

- title:

  The table title, or `NULL` for none.

## Value

a `gt_tbl` object (print it, or save it with
[`gt::gtsave()`](https://gt.rstudio.com/reference/gtsave.html)).

## Examples

``` r
results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
lactate_report(results)


  


Lactate thresholds
```
