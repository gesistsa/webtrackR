# Classify visits by matching to a list of classes

`classify_visits()` categorizes visits by either extracting the visit
URL's domain or host and matching them to a list of domains or hosts; or
by matching a list of regular expressions against the visit URL.

## Usage

``` r
classify_visits(
  wt,
  classes,
  match_by = "domain",
  regex_on = NULL,
  return_rows_by = NULL,
  return_rows_val = NULL
)
```

## Arguments

- wt:

  webtrack data object.

- classes:

  a data frame containing classes that can be matched to visits.

- match_by:

  character. Whether to match list entries from `classes` to the domain
  of a visit (`"domain"`) or the host (`"host"`) with an exact match; or
  with a regular expression against the whole URL of a visit
  (`"regex"`). If set to `"domain"` or `"host"`, both `wt` and `classes`
  need to have a column called accordingly. If set to `"regex"`, the
  `url` column of `wt` will be used, and you need to set `regex_on` to
  the column in `classes` for which to do the pattern matching. Defaults
  to `"domain"`.

- regex_on:

  character. Column in `classes` which to use for pattern matching.
  Defaults to `NULL`.

- return_rows_by:

  character. A column in `classes` on which to subset the returning
  data. Defaults to `NULL`.

- return_rows_val:

  character. The value of the columns specified in `return_rows_by`, for
  which data should be returned. For example, if your `classes` data
  contains a column `type`, which has a value called `"shopping"`,
  setting `return_rows_by` to `"type"` and `return_rows_val` to
  `"shopping"` will only return visits classified as `"shopping"`.

## Value

webtrack data.frame with the same columns as `wt` and any column in
`classes` except the column specified by `match_by`.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
data("domain_list")
wt <- as.wt_dt(testdt_tracking)
# classify visits via domain
wt_domains <- extract_domain(wt)
wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")
# classify visits via domain
# for the example, just renaming "domain" column
domain_list$host <- domain_list$domain
wt_hosts <- extract_host(wt)
wt_classes <- classify_visits(wt_hosts, classes = domain_list, match_by = "host")
# classify visits with pattern matching
# for the example, any value in "domain" treated as pattern
data("domain_list")
regex_list <- domain_list[type == "facebook"]
wt_classes <- classify_visits(wt[1:5000],
    classes = regex_list,
    match_by = "regex", regex_on = "domain"
)
# classify visits via domain and only return class "search"
data("domain_list")
wt_classes <- classify_visits(wt_domains,
    classes = domain_list,
    match_by = "domain", return_rows_by = "type",
    return_rows_val = "search"
)
} # }
```
