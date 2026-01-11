# An S3 class to store web tracking data

An S3 class to store web tracking data

Convert a data.frame containing web tracking data to a `wt_dt` object

## Usage

``` r
as.wt_dt(
  x,
  timestamp_format = "%Y-%m-%d %H:%M:%OS",
  tz = "UTC",
  varnames = c(panelist_id = "panelist_id", url = "url", timestamp = "timestamp")
)

is.wt_dt(x)
```

## Arguments

- x:

  data.frame containing a necessary set of columns, namely panelist's
  ID, visit URL and visit timestamp.

- timestamp_format:

  string. Specifies the raw timestamp's formatting. Defaults to
  `"%Y-%m-%d %H:%M:%OS"`.

- tz:

  timezone of date. defaults to UTC

- varnames:

  Named vector of column names, which contain the panelist's ID
  (`panelist_id`), the visit's URL (`url`) and the visit's timestamp
  (`timestamp`).

## Value

a webtrack data object with at least columns `panelist_id`, `url` and
`timestamp`

logical. TRUE if x is a webtrack data object and FALSE otherwise

## Details

A `wt_dt` table is a data.frame.

## Examples

``` r
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
is.wt_dt(wt)
#> [1] TRUE
```
