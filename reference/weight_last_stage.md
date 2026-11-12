# Time-weight the intensity of an incomplete last stage

The stage before it plus the completed share of the increment:
`completed + t / T * increment` (Kuipers et al., 2003).

## Usage

``` r
weight_last_stage(intensity, share)
```

## Arguments

- intensity:

  The intensities, in increasing order.

- share:

  The share of the last stage that was completed (`t / T`).

## Value

`intensity` with its last value replaced.
