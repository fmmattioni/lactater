# Demo data

A dataset containing the lactate and heart rate data collected from a
step-incremental test as an example, with a simulated rating of
perceived exertion

## Usage

``` r
demo_data
```

## Format

A data frame with 8 rows and 6 variables:

- step:

  the number of the step, starting at zero for the baseline.

- length:

  the length of each step.

- intensity:

  the intensity performed on each step (in this case in watts).

- lactate:

  the blood lactate concentration.

- heart_rate:

  the heart rate associated with that step.

- rpe:

  the rating of perceived exertion (Borg 6-20). Simulated: about 11 at 2
  mmol/L and 14 at 4 mmol/L, close to the means of 2,560 adults at the
  first rise in lactate (10.8) and at 4 mmol/L (14.1; Scherr et al.,
  2013), and 19 at the last stage.
