# Parse parts of path for text analysis

`parse_path()` parses parts of a path, i.e., anything separated by "/",
"-", "\_" or ".", and adds them as a new variable. Parts that do not
consist of letters only, or of a real word, can be filtered via the
argument `keep`.

## Usage

``` r
parse_path(wt, varname = "url", keep = "letters_only", decode = TRUE)
```

## Arguments

- wt:

  webtrack data object

- varname:

  character. name of the column from which to extract the host. Defaults
  to `"url"`.

- keep:

  character. Defines which types of path components to keep. If set to
  `"all"`, anything is kept. If `"letters_only"`, only parts containing
  letters are kept. If `"words_only"`, only parts constituting English
  words (as defined by the Word Game Dictionary, cf.
  https://cran.r-project.org/web/packages/words/index.html) are kept.
  Support for more languages will be added in future.

- decode:

  logical. Whether to decode the path (see
  [`utils::URLdecode()`](https://rdrr.io/r/utils/URLencode.html)),
  default to TRUE

## Value

webtrack data.frame with the same columns as wt and a new column called
`'path_split'` (or, if varname not equal to `'url'`,
`'<varname>_path_split'`) containing parts as a comma-separated string.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
wt <- parse_path(wt)
} # }
```
