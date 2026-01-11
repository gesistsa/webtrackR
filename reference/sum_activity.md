# Summarize activity per person

`sum_activity()` counts the number of active time periods (i.e., days,
weeks, months, years, or waves) by `panelist_id`. A period counts as
"active" if the panelist provided at least one visit for that period.

## Usage

``` r
sum_activity(wt, timeframe = "date")
```

## Arguments

- wt:

  webtrack data object.

- timeframe:

  character. Indicates for what time frame to aggregate visits. Possible
  values are `"date"`, `"week"`, `"month"`, `"year"` or `"wave"`. If set
  to `"wave"`, `wt` must contain a column call `wave`. Defaults to
  `"date"`.

## Value

a data.frame with columns `panelist_id`, column indicating the number of
active time units.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# summarize activity by day
wt_sum <- sum_activity(wt, timeframe = "date")
} # }
```
