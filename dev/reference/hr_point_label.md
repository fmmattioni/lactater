# The heart rate at a threshold, labeled beside its point

Its value over "at LT1", right of the point and under the line, where no
stage sits, or left of it and above near the right edge or the bottom of
the panel. The offsets are in points, so the label hugs its point at any
plot size. For internal use only.

## Usage

``` r
hr_point_label(x, y, lt, xlim, ylim, colours)
```

## Arguments

- x, y:

  The point: the threshold and its heart rate.

- lt:

  `"LT1"` or `"LT2"`.

- xlim, ylim:

  The limits of the panel.

- colours:

  The plot colours.

## Value

an `annotation_custom()` layer.
