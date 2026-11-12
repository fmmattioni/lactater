# The look of lactater's tables

Gives a gt table the look of the tables in lactater's reports and
articles, so your own tables can match them: IBM Plex Sans, the full
width of the page, bold column labels over one dark rule, light rules
between rows, bold row groups, and no fills or colors. Needs the gt
package. `options(lactater.web_font = FALSE)` leaves out the link to the
web font (IBM Plex Sans is then used when installed), which keeps a
self-contained page small.

## Usage

``` r
gt_lactater(table, size = 14)
```

## Arguments

- table:

  A gt table.

- size:

  The font size, in pixels.

## Value

the gt table.

## Examples

``` r
gt::gt(head(demo_data)) |>
  gt_lactater()


  

step
```
