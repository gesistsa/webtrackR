# webtrackR

``` r
library(webtrackR)
```

webtrackR is an R package to preprocess and analyze web tracking data,
i.e., web browsing histories of participants in an academic study. Web
tracking data is oftentimes collected and analyzed in conjunction with
survey data of the same participants.

`webtrackR` is part of a series of R packages to analyse webtracking
data:

- [webtrackR](https://github.com/gesistsa/webtrackR): preprocess raw
  webtracking data
- [domainator](https://github.com/schochastics/domainator): classify
  domains
- [adaR](https://github.com/gesistsa/adaR): parse urls

## Installation

You can install the development version of webtrackR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gesistsa/webtrackR")
```

The [CRAN](https://CRAN.R-project.org/package=webtrackR) version can be
installed with:

``` r
install.packages("webtrackR")
```

## S3 class `wt_dt`

The package defines an S3 class called `wt_dt` which inherits most of
the functionality from the `data.frame` class. A `summary` and `print`
method are included in the package.

Each row in a web tracking data set represents a visit. Raw data need to
have at least the following variables:

- `panelist_id`: the individual from which the data was collected
- `url`: the URL of the visit
- `timestamp`: the time of the URL visit

The function `as.wt_dt` assigns the class `wt_dt` to a raw web tracking
data set. It also allows you to specify the name of the raw variables
corresponding to `panelist_id`, `url` and `timestamp`. Additionally, it
turns the timestamp variable into `POSIXct` format.

All preprocessing functions check if these three variables are present.
Otherwise an error is thrown.

## Preprocessing

Several other variables can be derived from the raw data with the
following functions:

- [`add_duration()`](https://gesistsa.github.io/webtrackR/reference/add_duration.md)
  adds a variable called `duration` based on the sequence of timestamps.
  The basic logic is that the duration of a visit is set to the time
  difference to the subsequent visit, unless this difference exceeds a
  certain value (defined by argument `cutoff`), in which case the
  duration will be replaced by `NA` or some user-defined value (defined
  by `replace_by`).
- [`add_session()`](https://gesistsa.github.io/webtrackR/reference/add_session.md)
  adds a variable called `session`, which groups subsequent visits into
  a session until the difference to the next visit exceeds a certain
  value (defined by `cutoff`).
- [`extract_host()`](https://gesistsa.github.io/webtrackR/reference/extract_host.md),
  [`extract_domain()`](https://gesistsa.github.io/webtrackR/reference/extract_domain.md),
  [`extract_path()`](https://gesistsa.github.io/webtrackR/reference/extract_path.md),
  [`extract_query()`](https://gesistsa.github.io/webtrackR/reference/extract_query.md)
  extracts the host, domain, path, and query string of the raw URL and
  adds variables named accordingly. See function descriptions for
  definitions of these terms.
  [`drop_query()`](https://gesistsa.github.io/webtrackR/reference/drop_query.md)
  lets you drop the query and fragment components of the raw URL.
- [`add_next_visit()`](https://gesistsa.github.io/webtrackR/reference/add_next_visit.md)
  and
  [`add_previous_visit()`](https://gesistsa.github.io/webtrackR/reference/add_previous_visit.md)
  adds the previous or the next URL, domain, or host (defined by
  `level`) as a new variable.
- [`add_referral()`](https://gesistsa.github.io/webtrackR/reference/add_referral.md)
  adds a new variable indicating whether a visit was referred by a
  social media platform. Follows the logic of Schmidt et al.,
  [(2023)](https://doi.org/10.31235/osf.io/cks68).
- [`add_title()`](https://gesistsa.github.io/webtrackR/reference/add_title.md)
  downloads the title of a website (the text within the `<title>` tag of
  a web site’s `<head>`) and adds it as a new variable.
- [`add_panelist_data()`](https://gesistsa.github.io/webtrackR/reference/add_panelist_data.md).
  Joins a data set containing information about participants such as a
  survey.

## Classification

- [`classify_visits()`](https://gesistsa.github.io/webtrackR/reference/classify_visits.md)
  categorizes website visits by either extracting the URL’s domain or
  host and matching them to a list of domains or hosts, or by matching a
  list of regular expressions against the visit URL.

## Summarizing and aggregating

- [`deduplicate()`](https://gesistsa.github.io/webtrackR/reference/deduplicate.md)
  flags or drops (as defined by argument `method`) consecutive visits to
  the same URL within a user-defined time frame (as set by argument
  `within`). Alternatively to dropping or flagging visits, the function
  aggregates the durations of such duplicate visits.
- [`sum_visits()`](https://gesistsa.github.io/webtrackR/reference/sum_visits.md)
  and
  [`sum_durations()`](https://gesistsa.github.io/webtrackR/reference/sum_durations.md)
  aggregate the number or the durations of visits, by participant and by
  a time period (as set by argument `timeframe`). Optionally, the
  function aggregates the number / duration of visits to a certain class
  of visits.
- [`sum_activity()`](https://gesistsa.github.io/webtrackR/reference/sum_activity.md)
  counts the number of active time periods (defined by `timeframe`) by
  participant.

## Example code

A typical workflow including preprocessing, classifying and aggregating
web tracking data looks like this (using the in-built example data):

``` r
library(webtrackR)

# load example data and turn it into wt_dt
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)

# add duration
wt <- add_duration(wt)

# extract domains
wt <- extract_domain(wt)

# drop duplicates (consecutive visits to the same URL within one second)
wt <- deduplicate(wt, within = 1, method = "drop")

# load example domain classification and classify domains
data("domain_list")
wt <- classify_visits(wt, classes = domain_list, match_by = "domain")

# load example survey data and join with web tracking data
data("testdt_survey_w")
wt <- add_panelist_data(wt, testdt_survey_w)

# aggregate number of visits by day and panelist, and by domain class
wt_summ <- sum_visits(wt, timeframe = "date", visit_class = "type")
```
