# Produce data protocol skeleton

This is an internal function used in
[`run_data_input()`](https://fmmattioni.github.io/lactater/reference/run_data_input.md).

## Usage

``` r
helper_data_protocol(
  input_steps,
  input_length_steps,
  input_starting_load,
  input_step_increase,
  input_heart_rate_data,
  input_completed,
  input_last_length_step,
  sport
)
```

## Arguments

- input_steps:

  The total number of steps from the incremental test.

- input_length_steps:

  The length of each step, in minutes.

- input_starting_load:

  The starting load.

- input_step_increase:

  The step increase.

- input_heart_rate_data:

  A boolean indicating whether heart rate data was collected.

- input_completed:

  A boolean indicating whether the last step was fully completed.

- input_last_length_step:

  If the last step was not fully completed, then indicate how long it
  lasted.

- sport:

  The sport at which the incremental test was performed. One of
  `cycling`, `running`, or `swimming`.
