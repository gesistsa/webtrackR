# Add the next visit as a new column

`add_next_visit()` adds the subsequent visit, as determined by order of
timestamps as a new column. The next visit can be added as either the
full URL, the extracted host or the extracted domain, depending on
`level`.

## Usage

``` r
add_next_visit(wt, level = "url")
```

## Arguments

- wt:

  webtrack data object.

- level:

  character. Either `"url"`, `"host"` or `"domain"`. Defaults to
  `"url"`.

## Value

webtrack data.frame with the same columns as wt and a new column called
`url_next`,`host_next` or `domain_next`.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Adding next full URL as new column
wt <- add_next_visit(wt, level = "url")
# Adding next host as new column
wt <- add_next_visit(wt, level = "host")
# Adding next domain as new column
wt <- add_next_visit(wt, level = "domain")
} # }
```
