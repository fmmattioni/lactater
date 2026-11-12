# Layers of the Stegmann method

The fitted curve, the recovery samples placed after the last stage
(labeled with their minutes), the point where lactate is back at its
end-of-exercise value, and the tangent from there to the curve. For
internal use only.

## Usage

``` r
stegmann_layers(plot_data, colours)
```

## Arguments

- plot_data:

  The method's plot data, with the `curve` and the `tangent`.

- colours:

  The plot colours.

## Value

a list with the `layers`, the lactate `y` where the threshold sits, the
`top` and the `x_range`.
