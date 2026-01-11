# Add the previous visit as a new column

`add_previous_visit()` adds the previous visit, as determined by order
of timestamps as a new column The previous visit can be added as either
the full URL, the extracted host or the extracted domain, depending on
`level`.

## Usage

``` r
add_previous_visit(wt, level = "url")
```

## Arguments

- wt:

  webtrack data object.

- level:

  character. Either `"url"`, `"host"` or `"domain"`. Defaults to
  `"url"`.

## Value

webtrack data.frame with the same columns as wt and a new column called
`url_previous`,`host_previous` or `domain_previous.`.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Adding previous full URL as new column
wt <- add_previous_visit(wt, level = "url")
# Adding previous host as new column
wt <- add_previous_visit(wt, level = "host")
# Adding previous domain as new column
wt <- add_previous_visit(wt, level = "domain")
} # }
```
