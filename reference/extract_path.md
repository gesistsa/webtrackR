# Extract the path from URL

`extract_path()` adds the path of a URL as a new column. The path is
defined as the part following the host but not including a query
(anything after a "?") or a fragment (anything after a "#").

## Usage

``` r
extract_path(wt, varname = "url", decode = TRUE)
```

## Arguments

- wt:

  webtrack data object

- varname:

  character. name of the column from which to extract the host. Defaults
  to `"url"`.

- decode:

  logical. Whether to decode the path (see
  [`utils::URLdecode()`](https://rdrr.io/r/utils/URLencode.html)),
  default to TRUE

## Value

webtrack data.frame with the same columns as wt and a new column called
`'path'` (or, if varname not equal to `'url'`, `'<varname>_path'`)

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Extract path
wt <- extract_path(wt)
} # }
```
