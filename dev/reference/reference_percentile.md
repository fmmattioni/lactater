# The percentile of one value among the reference values

Interpolates the nine published percentiles to the age, then the value
between them on the normal scale.

## Usage

``` r
reference_percentile(value, method, unit, sex, age)
```

## Arguments

- value:

  The power (W) or power per kg.

- method:

  `"OBLA 3.0"` or `"Bsln + 1.0"`.

- unit:

  `"W"` or `"W/kg"`.

- sex:

  `"female"` or `"male"`.

- age:

  Age in years.

## Value

a list with the `percentile` (`NA` beyond the published ones) and the
`standing`.
