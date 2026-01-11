# Summarize visit duration by person

`sum_durations()` summarizes the duration of visits by person within a
`timeframe`, and optionally by `visit_class` of visit. Note:

- If for a time frame all rows are NA on the duration column, the
  summarized duration for that time frame will be NA.

- If only some of the rows of a time frame are NA on the duration
  column, the function will ignore those NA rows.

- If there were no visits to a class (i.e., a value of the 'visit_class'
  column) for a time frame, the summarized duration for that time frame
  will be zero; if there were visits, but NA on duration, the summarized
  duration will be NA.

## Usage

``` r
sum_durations(wt, var_duration = NULL, timeframe = NULL, visit_class = NULL)
```

## Arguments

- wt:

  webtrack data object.

- var_duration:

  character. Name of the duration variable if already present. Defaults
  to `NULL`, in which case duration will be approximated with
  `add_duration(wt, cutoff = 300, replace_by = "na", replace_val = NULL)`

- timeframe:

  character. Indicates for what time frame to aggregate visit durations.
  Possible values are `"date"`, `"week"`, `"month"`, `"year"`, `"wave"`
  or `NULL`. If set to `"wave"`, `wt` must contain a column call `wave`.
  Defaults to `NULL`, in which case the output contains duration of
  visits for the entire time.

- visit_class:

  character. Column that contains a classification of visits. For each
  value in this column, the output will have a column indicating the
  number of visits belonging to that value. Defaults to `NULL`.

## Value

a data.frame with columns `panelist_id`, column indicating the time unit
(unless `timeframe` set to `NULL`), `duration_visits` indicating the
duration of visits (in seconds, or whatever the unit of the variable
specified by `var_duration` parameter), and a column for each value of
`visit_class`, if specified.

## Examples
