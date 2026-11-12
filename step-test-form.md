# Lactate step test: record a test

# Lactate step test: record a test

[lactater](https://fmmattioni.github.io/lactater/)

Fill it in during the test; it saves itself in this browser. Then
download the file for lactater (or Excel). Nothing you type leaves your
computer.

New athlete, same protocol Clear everything

## Athlete

Name Date Time of day Sport Cycling Running Rowing Swimming Sex Female
Male Age (years) Body mass (kg)

## The 24 to 48 h before

Last meal (time, what) Caffeine (mg, time) Training in the last 48 h
Sleep, illness, other

## Protocol

Ergometer Start (W) Increment (W) Stage duration (min) Pause for
sampling (s) Treadmill grade (%) Sampling site earlobe fingertip venous
other Sample (as your analyzer measures it) Whole blood Plasma Sweat
Interstitial fluid Analyzer, calibrated at Room (°C, % humidity) RPE
scale 6-20 CR10 Tester

## Stages sample in the last 30 s of each stage, heart rate and RPE in the same window

| Stage | Intensity (W) | Clock time | Lactate (mmol/L) | Heart rate (bpm) | RPE | Notes |     |
|-------|---------------|------------|------------------|------------------|-----|-------|-----|

Add stage

## End of the test and recovery recovery samples are for Stegmann's threshold

Why it ended Time in the last stage (s) Peak heart rate (bpm)

| Minutes after the end | Lactate (mmol/L) |     |
|-----------------------|------------------|-----|

Add sample

## Notes

Download for lactater (.xlsx)

Stages only (.csv)

Print

New athlete, same protocol

Clear everything

## In R

    library(lactater)
    test <- read_recording("the-file.xlsx")
    results <- lactate_threshold(
      test$stages, intensity, lactate, heart_rate, rpe_column = rpe, rpe_scale = test$rpe_scale,
      sport = test$sport, sample = test$sample, plot = FALSE
    )
    test_report(results, athlete = test$athlete, protocol = test$protocol)

The .xlsx has three sheets: `stages` (the columns lactater reads, with
the resting sample in the first row), `recovery` and `test`. "New
athlete, same protocol" clears the athlete, the stages and the notes,
and keeps the protocol and the tester; "Clear everything" resets the
whole form.

Developed by [Felipe Mattioni Maturana](https://fmattioni.me).
