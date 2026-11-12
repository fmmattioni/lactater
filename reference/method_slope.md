# Keul and Simon

The individual anaerobic thresholds of Keul et al. (1979) and Simon et
al. (1981): the speed at which a 3rd degree polynomial through the
exercise stages rises by 1.26 mmol/L per km/h (an angle of 51°34') or by
1 mmol/L per km/h (45°). Both fitted lactate against running speed in
km/h, on treadmill tests at a 5% grade with 3-min stages of 2 km/h; a
slope in other units has no meaning, so these methods run on running
tests only. When the slope reaches the value more than once, the last
crossing where it rises through it is used. For internal use only.

## Usage

``` r
method_slope(data, method, slope)
```

## Arguments

- data:

  The data retrieved from
  [`prepare_data()`](https://fmmattioni.github.io/lactater/reference/prepare_data.md).

- method:

  The method name.

- slope:

  The slope (mmol/L per km/h).

## Value

a list with the result `rows` (method, fitting, intensity, lactate) and
the `plot_data`.
