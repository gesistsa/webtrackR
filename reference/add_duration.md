# Add time spent on a visit in seconds

`add_duration()` approximates the time spent on a visit based on the
difference between two consecutive timestamps, replacing differences
exceeding `cutoff` with the value defined in `replace_by`.

## Usage

``` r
add_duration(
  wt,
  cutoff = 300,
  replace_by = NA,
  last_replace_by = NA,
  device_switch_na = FALSE,
  device_var = NULL
)
```

## Arguments

- wt:

  webtrack data object.

- cutoff:

  numeric (seconds). If duration is greater than this value, it is reset
  to the value defined by `replace_by`. Defaults to 300 seconds.

- replace_by:

  numeric. Determines whether differences greater than the cutoff are
  set to `NA`, or some value. Defaults to `NA`.

- last_replace_by:

  numeric. Determines whether the last visit for an individual is set to
  `NA`, or some value. Defaults to `NA`.

- device_switch_na:

  boolean. Relevant only when data was collected from multiple devices.
  When visits are ordered by timestamp sequence, two consecutive visits
  can come from different devices, which makes the timestamp difference
  less likely to be the true duration. It may be preferable to set the
  duration of the visit to `NA` (`TRUE`) rather than the difference to
  the next timestamp (`FALSE`). Defaults to `FALSE`.

- device_var:

  character. Column indicating device. Required if 'device_switch_na'
  set to `TRUE`. Defaults to `NULL`.

## Value

webtrack data.frame with the same columns as wt and a new column called
for duration.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
wt <- add_duration(wt)
# Defining cutoff at 10 minutes, replacing those exceeding cutoff to 5 minutes,
# and setting duration before device switch to `NA`:
wt <- add_duration(wt,
    cutoff = 600, replace_by = 300,
    device_switch_na = TRUE, device_var = "device"
)
} # }
```
