# The thresholds that mark LT1 and LT2

What the caller asked for, otherwise what
[`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
was given, otherwise the package defaults (a result saved before the
choice was kept). For internal use only.

## Usage

``` r
chosen_pair(info, lt1 = NULL, lt2 = NULL)

pair_text(info)
```

## Arguments

- info:

  The `lactater` attribute of a result.

- lt1, lt2:

  A method, an intensity, or `NULL`.

## Value

a list with `lt1` and `lt2`.

`pair_text()`: "83 W (Bsln + 0.5)" for each of the two that was found,
named `LT1` and `LT2`.
