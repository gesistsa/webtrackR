# Extract the query from URL

`extract_query()` adds the query string of a URL as a new column. The
query is defined as the part following a "?" after the path but not
including a fragment (anything after a "#").

## Usage

``` r
extract_query(wt, varname = "url", decode = TRUE)
```

## Arguments

- wt:

  webtrack data object

- varname:

  character. name of the column from which to extract the host. Defaults
  to `"url"`.

- decode:

  logical. Whether to decode the query (see
  [`utils::URLdecode()`](https://rdrr.io/r/utils/URLencode.html)),
  default to TRUE

## Value

webtrack data.frame with the same columns as wt and a new column called
`'query'` (or, if varname not equal to `'url'`, `'<varname>_query'`)

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Extract query
wt <- extract_query(wt)
} # }
```
