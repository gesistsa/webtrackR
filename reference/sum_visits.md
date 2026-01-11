# Summarize number of visits by person

`sum_visits()` summarizes the number of visits by person within a
`timeframe`, and optionally by `visit_class` of visit.

## Usage

``` r
sum_visits(wt, timeframe = NULL, visit_class = NULL)
```

## Arguments

- wt:

  webtrack data object.

- timeframe:

  character. Indicates for what time frame to aggregate visits. Possible
  values are `"date"`, `"week"`, `"month"`, `"year"`, `"wave"` or
  `NULL`. If set to `"wave"`, `wt` must contain a column call `wave`.
  Defaults to `NULL`, in which case the output contains number of visits
  for the entire time.

- visit_class:

  character. Column that contains a classification of visits. For each
  value in this column, the output will have a column indicating the
  number of visits belonging to that value. Defaults to `NULL`.

## Value

a data.frame with columns `panelist_id`, column indicating the time unit
(unless `timeframe` set to `NULL`), `n_visits` indicating the number of
visits, and a column for each value of `visit_class`, if specified.

## Examples
