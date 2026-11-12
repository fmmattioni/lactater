# A curved arrow that ends on the edge of a dot

The arrow is laid out in inches when the plot is drawn, so it stops at
the edge of the dot and points at its center whatever the size of the
plot. For internal use only.

## Usage

``` r
pointer_layer(from, to, bend, colour, gap = 2.6)
```

## Arguments

- from, to:

  The label end and the dot center, in data units (ggplot2 applies the
  scale transformation).

- bend:

  How far the middle of the arrow bows out, as a share of its length;
  the sign sets the side.

- colour:

  The arrow color.

- gap:

  Where the arrow stops, in mm from the dot center.
