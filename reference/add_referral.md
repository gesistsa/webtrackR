# Add social media referrals as a new column

Identifies whether a visit was referred to from social media and adds it
as a new column. See details for method.

## Usage

``` r
add_referral(wt, platform_domains, patterns)
```

## Arguments

- wt:

  webtrack data object.

- platform_domains:

  character. A vector of platform domains for which referrers should be
  identified. Order and length must correspondent to `patterns` argument

- patterns:

  character. A vector of patterns for which referrers should be
  identified. Order and length must correspondent to `platform_domains`
  vector.

## Value

webtrack data.frame with the same columns as wt and a new column called
`referral`, which takes on NA if no referral has been identified, or the
name specified platform_domains if a referral from that platform has
been identified

## Details

To identify referrals, we rely on the method described as most valid in
Schmidt et al.: When the domain preceding a visit was to the platform in
question, and the query string of the visit's URL contains a certain
pattern, we count it as a referred visit. For Facebook, the pattern has
been identified by Schmidt et al. as `'fbclid='`, although this can
change in future.

## References

Schmidt, Felix, Frank Mangold, Sebastian Stier and Roberto Ulloa.
"Facebook as an Avenue to News: A Comparison and Validation of
Approaches to Identify Facebook Referrals". Working paper.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
wt <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
wt <- add_referral(wt,
    platform_domains = c("facebook.com", "twitter.com"),
    patterns = c("fbclid=", "utm_source=twitter")
)
} # }
```
