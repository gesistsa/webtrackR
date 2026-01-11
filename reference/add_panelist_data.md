# Add panelist features to tracking data

Adds information about panelists (e.g., from a survey) to the tracking
data.

## Usage

``` r
add_panelist_data(wt, data, cols = NULL, join_on = "panelist_id")
```

## Arguments

- wt:

  webtrack data object.

- data:

  a data frame containing panelist data which contains columns about
  panelists

- cols:

  character vector of columns to add. If `NULL`, all columns are added.
  Defaults to `NULL`.

- join_on:

  which columns to join on. Defaults to `"panelist_id"`.

## Value

webtrack object with the same columns and the columns from `data`
specified in `cols`.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
data("testdt_survey_w")
wt <- as.wt_dt(testdt_tracking)
# add survey test data
add_panelist_data(wt, testdt_survey_w)
} # }
```
