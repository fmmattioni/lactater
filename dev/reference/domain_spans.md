# The position text of the zones, once per domain

Zones 1 and 2 of the five-zone model are both "below LT1": the text is
drawn once, centered over the two.

## Usage

``` r
domain_spans(bands, span)
```

## Arguments

- bands:

  The zone bands of a plot: `from`, `to`, `description`.

- span:

  The width of the plot, in intensity.

## Value

a data frame with `from`, `to` and `description` (empty where there is
no room for it).
