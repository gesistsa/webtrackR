# Extract the domain from URL

`extract_domain()` adds the domain of a URL as a new column. By
"domain", we mean the "top private domain", i.e., the domain under the
public suffix (e.g., "`com`") as defined by the Public Suffix List. See
details.

Extracts the domain from urls.

## Usage

``` r
extract_domain(wt, varname = "url")
```

## Arguments

- wt:

  webtrack data object.

- varname:

  character. Name of the column from which to extract the host. Defaults
  to `"url"`.

## Value

webtrack data.frame with the same columns as wt and a new column called
`'domain'` (or, if varname not equal to `'url'`, `'<varname>_domain'`)

## Details

We define a "web domain" in the common colloquial meaning, that is, the
part of an web address that identifies the person or organization in
control. is `google.com`. More technically, what we mean by "domain" is
the "top private domain", i.e., the domain under the public suffix, as
defined by the Public Suffix List. Note that this definition sometimes
leads to counterintuitive results because not all public suffixes are
"registry suffixes". That is, they are not controlled by a domain name
registrar, but allow users to directly register a domain. One example of
such a public, non-registry suffix is `blogspot.com`. For a URL like
`www.mysite.blogspot.com`, our function, and indeed the packages we are
aware of, would extract the domain as `mysite.blogspot.com`, although
you might think of `blogspot.com` as the domain. For details, see
[here](https://github.com/google/guava/wiki/InternetDomainNameExplained)

## Examples

``` r
if (FALSE) { # \dontrun{
data("testdt_tracking")
wt <- as.wt_dt(testdt_tracking)
# Extract domain and drop rows without domain
wt <- extract_domain(wt)
# Extract domain and keep rows without domain
wt <- extract_domain(wt)
} # }
```
