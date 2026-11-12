# Method names with their subscripts

Plotmath, so that `"ModDmax"` is drawn as ModD with a subscript max and
`"LTratio"` as LT with a subscript ratio. Plotmath keeps the labels
plain `element_text()`, which users can restyle. `method_plotmath()`
gives the text to parse (for `label_parsed` and `parse = TRUE`),
`method_label()` the parsed expressions, and `method_html()` the same
names for HTML.

## Usage

``` r
method_plotmath(x)

method_html(x)

method_label(x, bold = FALSE)
```

## Arguments

- x:

  Method names.

- bold:

  Whether to draw them in bold, for titles.

## Value

a character vector, or an expression vector.
