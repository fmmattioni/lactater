# Convert intensity to pace

- swimming: speed in m/s to pace in s/100 m;

- running: speed in km/h to pace in s/km;

- rowing: power in W to split in s/500 m, with the Concept2 relation
  `watts = 2.80 / pace^3` (pace in s/m).

## Usage

``` r
convert_to_pace(intensity, sport = "swimming")
```

## Arguments

- intensity:

  The intensity to convert.

- sport:

  The sport.

## Value

the pace, rounded to 0.1 s.
