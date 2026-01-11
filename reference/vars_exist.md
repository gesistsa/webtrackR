# Check if columns are present

`vars_exist()` checks if columns are present in a webtrack data object.
By default, checks whether the data has a `panelist_id`, a `ulr` and a
`timestamp` column.#'

## Usage

``` r
vars_exist(wt, vars = c("panelist_id", "url", "timestamp"))
```

## Arguments

- wt:

  webtrack data object.

- vars:

  character vector of variables. Defaults to
  `c("panelist_id", "url", "timestamp")`.

## Value

A data.table object.
