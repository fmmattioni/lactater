# Test-retest reliability of each threshold, from your own repeated tests

The typical error of each threshold, from athletes who did the same test
two or more times while their fitness did not change (e.g. a few days
apart): the residual standard deviation of a two-way analysis of
athletes and tests, which leaves out the mean change from one test to
the next (Hopkins, 2000). It is given in intensity units and as a
coefficient of variation, `100 * (exp(s) - 1)`, with `s` the typical
error of the log intensities. Pass the result to `compare_tests(cv = )`
so that its verdicts use the noise of your own protocol, analyzer and
athletes rather than the default values, from trained male cyclists.

## Usage

``` r
lactate_reliability(x, athlete, test)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  with `.by`, one group per athlete and test (e.g.
  `.by = c("athlete", "test")`).

- athlete, test:

  The columns of `x` that name the athlete and the test (e.g. the test
  number or date). Tests are matched by their order within each athlete:
  first, second, and so on.

## Value

a tibble with one row per method: `method`, `n_athletes` (with two or
more tests), `n_tests`, `bias` (the mean change from one test to the
next, intensity units), `typical_error` (intensity units), `cv` (%) with
its 90% confidence interval (`cv_low`, `cv_high`), and the degrees of
freedom of the typical error (`df`). Methods with fewer than two such
athletes get `NA`.

## Details

A typical error from few athletes is itself uncertain: with 10 athletes
and 2 tests (9 degrees of freedom) its 90% confidence interval spans
0.73 to 1.65 times its value (from the chi-square distribution of a
variance). The result therefore gives the degrees of freedom (`df`) and
that interval for the `cv` (`cv_low`, `cv_high`), as reliability
statistics should always come with one (Sainani, 2017), and
[`compare_tests()`](https://fmmattioni.github.io/lactater/reference/compare_tests.md)
widens its intervals for a small `df`. Atkinson & Nevill (1998)
recommend more than 40 athletes for a reliability study; with fewer,
read the interval.

The `bias` is the mean change from each test to the next, which the
typical error leaves out. Report it beside the random error (Atkinson &
Nevill, 1998): a `bias` that is not near zero is a learning or order
effect, or a change in fitness between the tests. The `cv` assumes that
the error grows with the intensity, as it usually does for such
measures; with athletes of a similar level the two cannot be told apart,
and `typical_error` is the same noise in intensity units.

## References

Atkinson G, Nevill AM. Statistical methods for assessing measurement
error (reliability) in variables relevant to sports medicine. Sports
Medicine. 1998;26(4):217-238.

Hopkins WG. Measures of reliability in sports medicine and science.
Sports Medicine. 2000;30(1):1-15.

Sainani KL. Reliability statistics. PM&R. 2017;9(6):622-628.
doi:10.1016/j.pmrj.2017.05.001

## Examples

``` r
## the same athlete tested twice, a few days apart (simulated here as small changes in lactate)
set.seed(1)
repeats <- do.call(rbind, lapply(1:4, function(a) do.call(rbind, lapply(1:2, function(t) {
  transform(demo_data, athlete = a, test = t, intensity = intensity * (0.9 + 0.05 * a),
            lactate = lactate * exp(rnorm(nrow(demo_data), 0, 0.05)))
}))))
results <- lactate_threshold(repeats, intensity, lactate, method = c("OBLA", "Dmax"), plot = FALSE,
                             .by = c("athlete", "test"))
reliability <- lactate_reliability(results, "athlete", "test")
reliability
#> # A tibble: 10 × 9
#>    method      n_athletes n_tests  bias typical_error    cv cv_low cv_high    df
#>    <chr>            <int>   <int> <dbl>         <dbl> <dbl>  <dbl>   <dbl> <int>
#>  1 OBLA 2.0             4       8  0.25          3.43  3.16   1.95    9.52     3
#>  2 OBLA 2.5             4       8 -0.25          2.97  2.39   1.47    7.13     3
#>  3 OBLA 3.0             4       8  0.25          3.08  2.28   1.41    6.8      3
#>  4 OBLA 3.5             4       8  0.5           2.61  1.8    1.11    5.35     3
#>  5 OBLA 4.0             4       8  0.5           2.12  1.38   0.85    4.08     3
#>  6 Dmax                 4       8  1.25          3.39  2.5    1.54    7.48     3
#>  7 ModDmax              4       8  0             2.45  1.75   1.08    5.19     3
#>  8 Exp-Dmax             4       8 -0.25          1.86  1.29   0.8     3.82     3
#>  9 Log-Poly-M…          4       8  4.25          4.14  2.59   1.6     7.75     3
#> 10 Log-Exp-Mo…          4       8  4             3.87  2.39   1.48    7.15     3
```
