# Dmax

It applies the `Dmax` methods: `"Dmax"`, `"ModDmax"`, `"Exp-Dmax"`,
`"Log-Poly-ModDmax"`, and `"Log-Exp-ModDmax"`.

## Usage

``` r
method_dmax(data, curve, settings)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- curve:

  The user-selected curve from
  [`fit_curve()`](https://fmmattioni.github.io/lactater/reference/fit_curve.md).

- settings:

  The analysis settings.

## Value

a list with the result `rows` (method, fitting, intensity, lactate) and
the `plot_data` for each row.

## Details

Every variant looks for the point on the fitted curve that lies furthest
below a straight line between two anchors. The variants differ in the
fitted curve and in where the line starts:

- Dmax: 3rd degree polynomial, from the first stage.

- ModDmax: 3rd degree polynomial, from the stage preceding the first
  rise of at least `moddmax_rise` mmol/L.

- Exp-Dmax: exponential plus constant, from the first stage.

- LTP1-Poly-ModDmax: 3rd degree polynomial, from LTP1 (only when
  requested in `dmax_methods`).

- Log-Poly-ModDmax: 3rd degree polynomial, from the Log-log threshold.

- Log-Exp-ModDmax: exponential plus constant, from the Log-log
  threshold.

The line always ends at the last stage. The resting sample is never
used. With `settings$dmax_anchor = "observed"` the line's ends sit at
the measured lactate of the start and end stages instead of on the
fitted curve; the Log-log and LTP1 starts have no measured value, so
they always sit on the curve. `"original"` follows each paper: on the
curve for Dmax and Exp-Dmax, at the measured values for the ModDmax
variants.
