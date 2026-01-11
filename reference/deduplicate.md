# Deduplicate visits

`deduplicate()` flags, drops or aggregates duplicates, which are defined
as consecutive visits to the same URL within a certain time frame.

## Usage

``` r
deduplicate(
  wt,
  method = "aggregate",
  within = 1,
  duration_var = "duration",
  keep_nvisits = FALSE,
  same_day = TRUE,
  add_grpvars = NULL
)
```

## Arguments

- wt:

  webtrack data object.

- method:

  character. One of `"aggregate"`, `"flag"` or `"drop"`. If set to
  `"aggregate"`, consecutive visits (no matter the time difference) to
  the same URL are combined and their duration aggregated. In this case,
  a duration column must be specified via `"duration_var"`. If set to
  `"flag"`, duplicates within a certain time frame are flagged in a new
  column called `duplicate`. In this case, `within` argument must be
  specified. If set to `"drop"`, duplicates are dropped. Again, `within`
  argument must be specified. Defaults to `"aggregate"`.

- within:

  numeric (seconds). If `method` set to `"flag"` or `"drop"`, a
  subsequent visit is only defined as a duplicate when happening within
  this time difference. Defaults to 1 second.

- duration_var:

  character. Name of duration variable. Defaults to `"duration"`.

- keep_nvisits:

  boolean. If method set to `"aggregate"`, this determines whether
  number of aggregated visits should be kept as variable. Defaults to
  `FALSE`.

- same_day:

  boolean. If method set to `"aggregate"`, determines whether to count
  visits as consecutive only when on the same day. Defaults to `TRUE`.

- add_grpvars:

  vector. If method set to `"aggregate"`, determines whether any
  additional variables are included in grouping of visits and therefore
  kept. Defaults to `NULL`.

## Value

webtrack data.frame with the same columns as wt with updated duration

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
wt <- add_duration(wt, cutoff = 300, replace_by = 300)
# Dropping duplicates with one-second default
wt_dedup <- deduplicate(wt, method = "drop")
# Flagging duplicates with one-second default
wt_dedup <- deduplicate(wt, method = "flag")
# Aggregating duplicates
wt_dedup <- deduplicate(wt[1:1000], method = "aggregate")
# Aggregating duplicates and keeping number of visits for aggregated visits
wt_dedup <- deduplicate(wt[1:1000], method = "aggregate", keep_nvisits = TRUE)
# Aggregating duplicates and keeping "domain" variable despite grouping
wt <- extract_domain(wt)
wt_dedup <- deduplicate(wt, method = "aggregate", add_grpvars = "domain")
} # }
```
