# Extract the host from URL

`extract_host()` adds the host of a URL as a new column. The host is
defined as the part following the scheme (e.g., "https://") and
preceding the subdirectory (anything following the next "/"). Note that
for URL entries like `chrome-extension://soomething` or
`http://192.168.0.1/something`, result will be set to `NA`.

## Usage

``` r
extract_host(wt, varname = "url")
```

## Arguments

- wt:

  webtrack data object.

- varname:

  character. Name of the column from which to extract the host. Defaults
  to `"url"`.

## Value

webtrack data.frame with the same columns as wt and a new column called
`'host'` (or, if varname not equal to `'url'`, `'<varname>_host'`)

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Extract host and drop rows without host
wt <- extract_host(wt)
# Extract host and keep rows without host
wt <- extract_host(wt)
} # }
```
