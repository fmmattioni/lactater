# Thresholds against the population

Places the power of a cycling test at 3 mmol/L (`"OBLA 3.0"`) and at 1
mmol/L above the resting lactate (`"Bsln + 1.0"`) among people of the
same sex and age, on the reference percentiles of the German population
aged 14 to 64 (Fiedler et al., 2025): in watts and, with `body_mass`, in
watts per kg.

## Usage

``` r
reference_percentiles(x, age, sex, body_mass = NULL)
```

## Arguments

- x:

  The result of
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  for a cycling test.

- age:

  Age in years, from 14 to 64.

- sex:

  `"female"` or `"male"` (or `"F"`, `"M"`).

- body_mass:

  Body mass (kg), for the percentiles of power per kg, or `NULL`.

## Value

a `lactate_reference` tibble with one row per threshold and unit:
`method`, `value`, `unit`, the `percentile`, and the `standing` in plain
words, from the published percentiles it falls between: "top 5%", "top
10%", "top 25%", "upper half", "lower half", "bottom 25%", "bottom 10%",
"bottom 5%", or beyond the 2.5th and 97.5th, "top 2.5%" and "bottom
2.5%".

## Details

The percentiles come from 2,389 women and 2,801 men of two national
health surveys (KiGGS wave 2 and DEGS1), tested on a cycle ergometer
from 25 W with 25 W more every 2 min, at 60-80 rpm, with earlobe blood
analyzed on a Biosen C-Line, until 85% of the age-predicted maximal
heart rate. Their thresholds were computed with lactater "on a 3rd
degree polynomial with the baseline (step 0) data" included, so the two
thresholds are computed here in the same way, whatever `fit` and
`include_baseline` the test was analyzed with: they can differ by a few
watts from its table. Read the result with three limits in mind:

- **The protocol.** Percentiles transfer only between tests run the same
  way. Shorter stages put the thresholds at higher intensities (Bentley
  et al., 2007), so a test with 3-min stages looks a little weaker
  against these 2-min norms. A warning is given when the steps of the
  test are not 25 W, it has no resting sample, or the `stage_duration`
  or `site` given to
  [`lactate_threshold()`](https://fmmattioni.github.io/lactater/reference/lactate_threshold.md)
  are not 2 min and earlobe; without them, the stage length and the
  sampling site are yours to check.

- **The population.** These are values of the general population, not of
  athletes: most trained cyclists land above the 97.5th percentile.

- **The precision.** The percentile is interpolated between the nine
  published percentiles, from the 2.5th to the 97.5th, on the normal
  scale and at the exact age. Beyond the 2.5th or the 97.5th it is `NA`,
  and `standing` says "top 2.5%" or "bottom 2.5%".

## References

Fiedler J, Thron M, Altmann S, Haertel S, Barsch F, Schlenker L, Krug S,
Boes K, Woll A. Reference standards for power at the lactate threshold 2
for cycle ergometry throughout the lifespan in Germany. Journal of
Sports Sciences. 2025;43(16):1654-1664.
doi:10.1080/02640414.2025.2517913

Bentley DJ, Newell J, Bishop D. Incremental exercise test design and
analysis: implications for performance diagnostics in endurance
athletes. Sports Medicine. 2007;37(7):575-586.

## Examples

``` r
results <- lactate_threshold(demo_data, intensity, lactate, heart_rate, plot = FALSE)
reference_percentiles(results, age = 31, sex = "male", body_mass = 72)
#> # Reference percentiles: men aged 31 in Germany (Fiedler et al., 2025)
#> # Reference test:        cycle ergometer, +25 W every 2 min, earlobe blood;
#> #   general population, not athletes
#>   method      value unit  percentile standing  
#>   <chr>       <dbl> <chr>      <dbl> <chr>     
#> 1 OBLA 3.0   130    W             53 upper half
#> 2 Bsln + 1.0 105    W             41 lower half
#> 3 OBLA 3.0     1.8  W/kg          76 top 25%   
#> 4 Bsln + 1.0   1.45 W/kg          63 upper half
```
