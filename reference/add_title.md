# Download and add the "title" of a URL

Gets the title of a URL by accessing the web address online and adds the
title as a new column. See details for the meaning of "title". You need
an internet connection to run this function.

## Usage

``` r
add_title(wt, lang = "en-US,en-GB,en")
```

## Arguments

- wt:

  webtrack data object.

- lang:

  character (a language tag). Language accepted by the request. Defaults
  to `"en-US,en-GB,en"`. Note that you are likely to still obtain titles
  different from the ones seen originally by the user, because the
  language also depend on the user's IP and device settings.

## Value

webtrack data.frame with the same columns as wt and a new column called
`"title"`, which will be `NA` if the title cannot be retrieved.

## Details

The title of a website (the text within the `<title>` tag of a web
site's `<head>`) \#' is the text that is shown on the "tab" when looking
at the website in a browser. It can contain useful information about a
URL's content and can be used, for example, for classification purposes.
Note that it may take a while to run this function for a large number of
URLs.

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)[1:2]
# Get titles with `lang` set to default English
wt_titles <- add_title(wt)
# Get titles with `lang` set to German
wt_titles <- add_title(wt, lang = "de")
} # }
```
