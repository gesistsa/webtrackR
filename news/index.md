# Changelog

## webtrackR 0.3.2

- fixed undesired copy of data.table objects
  [\#101](https://github.com/gesistsa/webtrackR/issues/101)
- bump `data.table` dependency to \>= 1.18.0

## webtrackR 0.3.1

CRAN release: 2024-04-30

- fixed CRAN problems with tests

## webtrackR 0.3.0

CRAN release: 2024-04-28

- revert back to new version of data.table

## webtrackR 0.2.0

- reimplemented in base R removing several dependencies
- fixed [\#1](https://github.com/gesistsa/webtrackR/issues/1),
  [\#81](https://github.com/gesistsa/webtrackR/issues/81),
  [\#82](https://github.com/gesistsa/webtrackR/issues/82),
  [\#83](https://github.com/gesistsa/webtrackR/issues/83),
  [\#84](https://github.com/gesistsa/webtrackR/issues/84),
  [\#87](https://github.com/gesistsa/webtrackR/issues/87),
  [\#88](https://github.com/gesistsa/webtrackR/issues/88),
  [\#98](https://github.com/gesistsa/webtrackR/issues/98)

## webtrackR 0.1.0

CRAN release: 2023-08-31

- added preprocessing functions
  [`add_session()`](https://gesistsa.github.io/webtrackR/reference/add_session.md),
  [`extract_host()`](https://gesistsa.github.io/webtrackR/reference/extract_host.md),
  [`extract_domain()`](https://gesistsa.github.io/webtrackR/reference/extract_domain.md),
  [`extract_path()`](https://gesistsa.github.io/webtrackR/reference/extract_path.md),
  [`add_next_visit()`](https://gesistsa.github.io/webtrackR/reference/add_next_visit.md),
  [`add_previous_visit()`](https://gesistsa.github.io/webtrackR/reference/add_previous_visit.md),
  [`add_title()`](https://gesistsa.github.io/webtrackR/reference/add_title.md),
  [`add_referral()`](https://gesistsa.github.io/webtrackR/reference/add_referral.md)
- changed example data to something more realistic
- added unit tests
  ([\#65](https://github.com/gesistsa/webtrackR/issues/65))
- added filtering, summarizing, and aggregating functions

## webtrackR 0.0.1

CRAN release: 2023-03-13

- non documented functions
