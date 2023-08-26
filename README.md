
<!-- README.md is generated from README.Rmd. Please edit that file -->

# webtrackR <img src="man/figures/logo.png" width="120px" align="right"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/webtrackR)](https://CRAN.R-project.org/package=webtrackR)
[![R-CMD-check](https://github.com/schochastics/webtrackR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/schochastics/webtrackR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/schochastics/webtrackR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/schochastics/webtrackR?branch=main)
<!-- badges: end -->

webtrackR is an R package to preprocess and analyse web tracking data (also in conjunction with survey data of participants). The package is built on top of data.table and can thus comfortably handle very large datasets. 

## Installation

You can install the development version of webtrackR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("schochastics/webtrackR")
```

## S3 class `wt_dt`

The package defines an S3 class called `wt_dt` which inherits most of the functionality from the `data.table` class. A `summary` and `print` method are included in the package.

Each row in a web tracking data set represents a visit. Raw data are assumed to have at least the following variables:

- `panelist_id`: the individual from which the data was collected
- `url`: the URL of the visit
- `timestamp`: the time of the URL visit

The function `as.wt_dt` assigns the `wt_dt` to raw data set. It also allows you to specify the name of the raw variables corresponding to `panelist_id`, `url` and `timestamp`. It also turns the timestamp variable into `POSIXct` format.

All preprocessing functions check if these three variables are present. Otherwise an error is thrown.

## Preprocessing

Several other variables can be derived from the raw data with the following functions:

- `add_duration()` adds a variable called `duration`, based on the sequence of timestamps. The basic logic is that the duration of a visit is set to the time difference to the subsequent visit, unless this difference exceeds a certain value (defined by argument `cutoff`), in which case the duration will be replaced by `NA` or some user-defined value (as set by `replace_by`). 
- `add_session()` adds a variable called `session`, which groups subsequent visits into a session until the difference to the next visit exceeds a certain value (defined by `cutoff`). 
- `extract_host()`, `extract_domain()`, `extract_path()` lets you extract the host, domain and path of the raw URL, and adds variables named accordingly. See function descriptions for definitions of these terms. `drop_query()` lets you drop the query and fragment components of the raw URL.
- `add_next_visit()` and `add_previous_visit()` adds the previous or the next URL (or domain, or host) as a new variable.
- `add_title()` downloads the title (the text within the `<title>` tag of a web site's `<head>`) and adds it as a new variable.
- `add_referral()` adds a new variable indicating whether a visit was referred to by a social media platform. Follows the logic of Schmidt et al. (forthcoming).
- `add_panelist_data()`. Joins a data set containing information about panelists such as a survey.

## Classification

- `classify_visits()` allows you to 

## Filtering / summarizing / aggregating

- `deduplicate()` allows to flag or drop (as defined by argument `method`) consecutive visits to the same URL within a user-defined time frame (as set by argument `within`). Alternatively to dropping or flagging visits, you can aggregate the durations of such duplicate visits.
- `With sum_visits()` and `sum_durations()` you can aggregate the number of visits, or aggregate the durations of visits, by panelist and by a time period (as set by argument `timeframe`). Optionally, you can also aggregate the number / duration of visits to a certain class of visits.
- `sum_activity()` counts the number of active time periods (as set by argument `timeframe`) by panelist.

A typical workflow including preprocessing, classifying and aggregating looks like this (using the in-built example data):

``` r
library(webtrackR)

# load example data and make into wt_dt
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)

# add duration
wt <- add_duration(wt)

# extract domains
wt <- extract_domain(wt)

# drop duplicates (consecutive visits to the same URL within one second)
wt_dedup <- deduplicate(wt, within = 1, method = "drop")

# load example domain classification and classify domains with list
data("domain_list")
wt <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")

# load example survey data and join to tracking data
data("testdt_survey_w")
wt <- add_panelist_data(wt, testdt_survey_w)

# aggregate number of visits by day and panelist, and by domain class
wt_summ <- sum_visits(wt, timeframe = "date", visit_class = "type")

```
## Audience Networks

The package also contains functions to explore audience networks. # DAVID, IST DAS SO NOCH AKTUELL?

``` r
audience_network(wt, cutoff = 3, type = "pmi")
```

- `cutoff` indicates minimal duration to count as visit.
- `type` can be one of “pmi”, “phi”, “disparity”, “sdsm”, or “fdsm”


### Ideology

Top 500 Bakshy scores are available in the package # SOLLEN WIR DAS ERSTMAL RAUSLASSEN?

``` r
data("bakshy")
```




