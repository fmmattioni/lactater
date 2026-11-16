# Layers of a method drawn on the fitted curve

The curve, plus the Dmax line and the gap to it, the Bsln+ baseline and
rise, or the IAT rise from LTratio. For internal use only.

## Usage

``` r
curve_layers(plot_data, intensity, lactate, colours, sport)
```

## Arguments

- plot_data:

  What the method drew on: the fitted `curve` (with a Dmax `line`, a
  Bsln+ `baseline`, or the IAT `from` point), a `breakpoints` fit, or a
  `ratio` curve, and the `fitting` label.

- colours:

  The plot colours.

- sport:

  The sport, for the unit of the LTratio label.

## Value

a list with the `layers`, the lactate `y` where the threshold sits, and
the `top` of the drawing.
