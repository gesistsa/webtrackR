# Add a session variable

`add_session()` groups visits into "sessions", defining a session to end
when the difference between two consecutive timestamps exceeds a
`cutoff`.

## Usage

``` r
add_session(wt, cutoff)
```

## Arguments

- wt:

  webtrack data object.

- cutoff:

  numeric (seconds). If the difference between two consecutive
  timestamps exceeds this value, a new browsing session is defined.

## Value

webtrack data.frame with the same columns as wt and a new column called
session.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Setting cutoff to 30 minutes
wt <- add_session(wt, cutoff = 1800)
} # }
```
