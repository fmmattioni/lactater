# lactater ggplot2 theme

The theme used by every lactater plot: axis lines, no grid, and quiet
text, so the lactate curve and the threshold carry the color. Use it to
make your own plots match.

## Usage

``` r
theme_lactater(
  base_size = 11,
  base_family = getOption("lactater.font", ""),
  style = getOption("lactater.style", "light")
)
```

## Arguments

- base_size:

  Base font size, in points.

- base_family:

  Base font family.

- style:

  `"light"`, `"night"` or `"journal"`.

## Value

a `ggplot2` theme.

## Details

Two options change every lactater plot:

- `options(lactater.style = "night")` switches to a dark version, and
  `"journal"` to a quieter print look (an ink curve with one amber
  accent). `"light"`, a blue curve with a green threshold, is the
  default.

- `options(lactater.font = "IBM Plex Sans")` sets the font of the text
  and the annotations. The font must be installed (see
  [`systemfonts::require_font()`](https://systemfonts.r-lib.org/reference/require_font.html)).

The subtitle and the caption are text boxes from ggtext, so they wrap by
themselves to the width of the plot. Restyle them with
[`ggtext::element_textbox_simple()`](https://wilkelab.org/ggtext/reference/element_textbox.html)
rather than
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
which cannot replace them.

Plots are `ggplot2` objects, so anything else can be changed by adding
layers, scales or a
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Examples

``` r
library(ggplot2)
ggplot(demo_data, aes(intensity, lactate)) +
  geom_point() +
  theme_lactater()
```
