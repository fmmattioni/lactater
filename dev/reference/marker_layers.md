# The threshold: dot, dotted guides to both axes, and the label joined by a curved arrow

The label sits above the threshold, to the left when there is room
(above a rising curve the plot is empty) and to the right otherwise;
with `below`, below the curve and to the right, when the threshold
leaves room under it. A label steps over (or, below the curve, under)
the lines of `avoid`, the Dmax line and the drawn curve, when one would
cross it. For internal use only.

## Usage

``` r
marker_layers(
  x,
  y,
  label,
  x_range,
  y_range,
  log_axes,
  colours,
  below = FALSE,
  avoid = list()
)
```

## Arguments

- x, y:

  Where the threshold sits.

- label:

  A list with the `value` and the `detail` lines.

- x_range, y_range:

  The ranges of the plot.

- log_axes:

  Whether both axes are logarithmic.

- colours:

  The plot colours.

- below:

  Whether to put the label below the curve.

- avoid:

  The lines (`intensity`, `lactate`) the label must not cross: the Dmax
  line and the drawn curve.
