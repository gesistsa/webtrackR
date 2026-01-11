# Drop the query and fragment from URL

`drop_query()` adds the URL without query and fragment as a new column.
The query is defined as the part following a "?" after the path. The
fragement is anything following a "#" after the query.

## Usage

``` r
drop_query(wt, varname = "url")
```

## Arguments

- wt:

  webtrack data object.

- varname:

  character. name of the column from which to extract the host. Defaults
  to `"url"`.

## Value

webtrack data.frame with the same columns as wt and a new column called
`'<varname>_noquery'`

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Extract URL without query/fragment
wt <- drop_query(wt)
} # }
```
