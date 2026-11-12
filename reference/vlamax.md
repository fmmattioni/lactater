# Maximal lactate accumulation rate (VLamax)

Estimates the maximal lactate accumulation rate (VLamax) from a short
all-out sprint test (typically 15 s), not from the incremental test:

## Usage

``` r
vlamax(lactate_pre, lactate_post, test_duration = 15, alactic_time = 3.5)
```

## Arguments

- lactate_pre:

  Blood lactate before the sprint (mmol/L).

- lactate_post:

  Blood lactate samples after the sprint (mmol/L); the highest is used.

- test_duration:

  Sprint duration in seconds. Defaults to `15`.

- alactic_time:

  Alactic time in seconds. Defaults to `3.5` (Meixner et al., 2024).

## Value

a tibble with `vlamax` and `vlapeak` (mmol/L/s), `lactate_peak` and
`delta_lactate` (mmol/L), and `glycolytic_time` (s).

## Details

\$\$VLamax = (La\_{peak} - La\_{pre}) / (t\_{test} - t\_{alactic})\$\$

VLamax needs its own protocol: a resting lactate sample before the
sprint, an all-out sprint, and lactate sampled every minute for several
minutes after it (Meixner et al. (2024) used up to 8 min). The highest
post-sprint value is the peak.

The alactic time (`alactic_time`) is the part of the sprint assumed to
be covered by phosphocreatine. Fixing it at 3.5 s gave the most reliable
VLamax in 50 cyclists (ICC 0.91, CV 3.1%); estimating it from the power
curve was much less reliable (CV 12-26%), and a 1-s change in it can
change VLamax by up to 26% (Meixner et al., 2024). Protocols vary a lot
between studies (Langley et al., 2025), and whether VLamax measures the
maximal glycolytic rate is still debated: Clark & Macdermid (2025) found
it adds little beyond the sprint power itself. Compare values only
within the same protocol, ergometer and lactate analyzer.

The default `test_duration` of 15 s is the sprint of Meixner et al.
(2024) and of most cycling studies. A review of 60 studies recommends a
sport-specific all-out sprint of 10-12 s instead, with lactate sampled
every minute for 10 min, and calls the rate from 15 s or longer a peak
rather than a maximal rate; it also found the use of VLamax to predict
performances of 1 min or longer inconclusive (Quittmann, 2026). Set
`test_duration` to your sprint.

`vlapeak` is the same rise divided by the whole sprint, with no alactic
time, \\vLa\_{peak} = (La\_{peak} - La\_{pre}) / t\_{test}\\, so it is
lower than `vlamax`. Wackerhage et al. (2025) proposed it partly because
the alactic-time correction is error-prone. In 182 cyclists and
triathletes, both gave similar associations with the lactate thresholds
(Röhrs et al., 2026).

## References

Meixner B, Nusser V, Koehler K, Sablain M, Boone J, Sperlich B.
Reliability of power output, maximal rate of capillary blood lactate
accumulation, and phosphagen contribution time following 15-s sprint
cycling in amateur cyclists. Physiological Reports. 2024;12(10):e16086.
doi:10.14814/phy2.16086

Langley J, Haase R, Nitzsche N, Porter M. Methodological approaches in
testing maximal lactate accumulation rate - vLamax: a systematic review.
Journal of Science and Cycling. 2025;14(1):9. doi:10.28985/1425.jsc.09

Clark B, Macdermid PW. VLamax correlates strongly with glycolytic
performance. Research Quarterly for Exercise and Sport. 2025:1-8.

Quittmann OJ. Maximal lactate accumulation rate (cLamax): current
evidence and future directions for exercise testing and training.
European Journal of Applied Physiology. 2026;126(1):1-36.
doi:10.1007/s00421-025-06022-7

Röhrs D, Wagner CM, Reer R, Keiner M. Lactate accumulation rate is
associated with lactate turnpoints in cyclists and triathletes: a
comparison of vLapeak and vLamaxblood. European Journal of Applied
Physiology. 2026. doi:10.1007/s00421-026-06444-x

Wackerhage H, Kabasakalis A, Seiler S, Heck H. Is the vLamax for
glycolysis what the VO2max is for oxidative phosphorylation? Sports
Medicine. 2025;55(8):1853-1866. doi:10.1007/s40279-025-02259-6

## Examples

``` r
vlamax(lactate_pre = 1.1, lactate_post = c(8.9, 10.4, 10.9, 10.6, 10.1))
#> # A tibble: 1 × 5
#>   vlamax vlapeak lactate_peak delta_lactate glycolytic_time
#>    <dbl>   <dbl>        <dbl>         <dbl>           <dbl>
#> 1  0.852   0.653         10.9           9.8            11.5
```
