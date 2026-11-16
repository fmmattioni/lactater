# Dmax point

Finds where the curve lies furthest below the straight line from
`(x0, y0)` to `(x1, y1)`. The perpendicular distance is the vertical
distance times a constant, so both peak at the same intensity. For
internal use only.

## Usage

``` r
dmax_point(curve, x0, y0, x1, y1)
```

## Arguments

- curve:

  A function returning the fitted lactate at given intensities.

- x0, y0:

  The start of the line.

- x1, y1:

  The end of the line.

## Value

the intensity, or `NA` when the curve never dips below the line.
