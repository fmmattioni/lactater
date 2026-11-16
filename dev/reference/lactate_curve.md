# Lactate curve

It retrieves the lactate curve for plotting purposes.

## Usage

``` r
lactate_curve(
  .data,
  intensity_column,
  lactate_column,
  heart_rate_column,
  fit = c("3rd degree polynomial", "4th degree polynomial", "B-spline"),
  include_baseline = FALSE,
  sport = c("cycling", "running", "swimming", "rowing")
)
```

## Arguments

- .data:

  The raw data.

- intensity_column:

  The name of the intensity column.

- lactate_column:

  The name of the lactate column.

- heart_rate_column:

  The name of the heart rate column, if applicable.

- fit:

  The fit you would like to use for plotting the lactate curve. Options
  are `3rd degree polynomial`, `4th degree polynomial`, or `B-spline`.

- include_baseline:

  A boolean to indicate whether to include the resting (baseline) sample
  in the fit. The resting sample is the row with intensity 0; it is
  placed one median step before the first stage. Data without a resting
  sample is fine. Rows with a missing intensity or lactate are dropped
  with a warning.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.

## Value

a list with the following elements:

- data:

  a tibble containing the raw data with the columns `intensity`,
  `lactate`, and `heart_rate`.

- lactate_curve:

  a tibble containing the data with the columns `intensity` and
  `lactate` for plotting the lactate curve according to the `fit` method
  chosen.

- heart_rate_response:

  a tibble with the columns `intensity` and `heart_rate` at each
  exercise stage. A line through these points is the interpolation used
  for the heart rate at each threshold.

## Examples

``` r
lactate_curve(demo_data, intensity, lactate, heart_rate)
#> $data
#> # A tibble: 8 × 3
#>   intensity lactate heart_rate
#>       <dbl>   <dbl>      <int>
#> 1        25    0.93         96
#> 2        50    0.98        114
#> 3        75    1.23        134
#> 4       100    1.88        154
#> 5       125    2.8         170
#> 6       150    4.21        182
#> 7       175    6.66        193
#> 8       191    8.64        198
#> 
#> $lactate_curve
#> # A tibble: 1,411 × 2
#>    intensity lactate
#>        <dbl>   <dbl>
#>  1      50     0.957
#>  2      50.1   0.959
#>  3      50.2   0.960
#>  4      50.3   0.961
#>  5      50.4   0.963
#>  6      50.5   0.964
#>  7      50.6   0.965
#>  8      50.7   0.966
#>  9      50.8   0.968
#> 10      50.9   0.969
#> # ℹ 1,401 more rows
#> 
#> $heart_rate_response
#> # A tibble: 7 × 2
#>   intensity heart_rate
#>       <int>      <int>
#> 1        50        114
#> 2        75        134
#> 3       100        154
#> 4       125        170
#> 5       150        182
#> 6       175        193
#> 7       191        198
#> 
```
