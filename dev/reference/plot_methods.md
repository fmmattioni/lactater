# Combine lactate threshold methods into one plot

The one-line note under each title is dropped: it would crowd a grid of
small panels.

## Usage

``` r
plot_methods(plots, ...)
```

## Arguments

- plots:

  The `ggplot2` objects to be combined.

- ...:

  Additional arguments passed onto
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html).

## Value

a `patchwork` object
