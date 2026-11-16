# Refine the extremum of a function found on a grid

The grid finds the basin of the extremum;
[`optimize()`](https://rdrr.io/r/stats/optimize.html) between the
neighbours of the best grid point makes it exact. For internal use only.

## Usage

``` r
refine_extremum(f, grid, values = f(grid), maximum = FALSE, tol = 1e-08)
```

## Arguments

- f:

  The function of one variable.

- grid:

  Where it was evaluated.

- values:

  `f(grid)`, when already computed.

- maximum:

  Whether to look for a maximum.

- tol:

  The tolerance of
  [`optimize()`](https://rdrr.io/r/stats/optimize.html).

## Value

the result of [`optimize()`](https://rdrr.io/r/stats/optimize.html):
`minimum` or `maximum`, and `objective`.
