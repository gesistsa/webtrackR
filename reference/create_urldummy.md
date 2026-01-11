# Create an urldummy variable

Create an urldummy variable

## Usage

``` r
create_urldummy(wt, dummy, name)
```

## Arguments

- wt:

  webtrack data object

- dummy:

  a vector of urls that should be dummy coded

- name:

  name of dummy variable to create.

## Value

webtrack object with the same columns and a new column called "name"
including the dummy variable

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
wt <- extract_domain(wt)
code_urls <- "https://dkr1.ssisurveys.com/tzktsxomta"
create_urldummy(wt, dummy = code_urls, name = "test_dummy")
} # }
```
