#' Classify visits by matching to a list of classes
#' @description
#' `classify_visits()` categorizes visits by either extracting the visit URL's
#' domain or host and matching them to a list of domains or hosts;
#' or by matching a list of regular expressions against the visit URL.
#' @param wt webtrack data object.
#' @param classes a data.table containing classes that can be matched to visits.
#' @param match_by character. Whether to match list entries from `classes` to
#' the domain of a visit (`"domain"`) or the host (`"host"`) with an exact match;
#' or with a regular expression against the whole URL of a visit (`"regex"`).
#' If set to `"domain"` or `"host"`, both `wt` and `classes` need to have
#' a column called accordingly. If set to `"regex"`, the `url` column of `wt`
#' will be used, and you need to set `regex_on` to the column in `classes`
#' for which to do the pattern matching. Defaults to `"domain"`.
#' @param regex_on character. Column in `classes` which to use for
#' pattern matching. Defaults to `NULL`.
#' @param return_rows_by character. A column in `classes` on which to
#' subset the returning data. Defaults to `NULL`.
#' @param return_rows_val character. The value of the columns specified in
#' `return_rows_by`, for which data should be returned. For example, if your
#' `classes` data contains a column `type`, which has a value called `"shopping"`,
#' setting `return_rows_by` to `"type"` and `return_rows_val` to `"shopping"`
#' will only return visits classified as `"shopping"`.
#' @importFrom data.table is.data.table setnames
#' @return webtrack data.table with the same columns as `wt` and any column
#' in `classes` except the column specified by `match_by`.
#' @examples
#' data("testdt_tracking")
#' data("domain_list")
#' wt <- as.wt_dt(testdt_tracking)
#' # classify visits via domain
#' wt_domains <- extract_domain(wt, drop_na = FALSE)
#' wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")
#' # classify visits via domain
#' # for the example, just renaming "domain" column
#' domain_list$host <- domain_list$domain
#' wt_hosts <- extract_host(wt, drop_na = FALSE)
#' wt_classes <- classify_visits(wt_hosts, classes = domain_list, match_by = "host")
#' # classify visits with pattern matching
#' # for the example, any value in "domain" treated as pattern
#' data("domain_list")
#' regex_list <- domain_list[type == "facebook"]
#' wt_classes <- classify_visits(wt[1:5000], classes = regex_list, match_by = "regex", regex_on = "domain")
#' # classify visits via domain and only return class "search"
#' data("domain_list")
#' wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain", return_rows_by = "type", return_rows_val = "search")
#' @export
classify_visits <- function(wt, classes, match_by = "domain",
                            regex_on = NULL,
                            return_rows_by = NULL,
                            return_rows_val = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  if (!is.data.table(classes)) {
    stop("classes needs to be a data.table")
  }
  match.arg(match_by, c("domain", "host", "regex"))
  if (match_by == "domain") {
    vars_exist(wt, vars = c("domain"))
    stopifnot("couldn't find the column 'domain' in the classes data" = "domain" %in% names(classes))
    wt <- classes[wt, on = "domain"]
  } else if (match_by == "host") {
    vars_exist(wt, vars = c("host"))
    stopifnot("couldn't find the column 'host' in the classes data" = "host" %in% names(classes))
    wt <- classes[wt, on = "host"]
  } else if (match_by == "regex") {
    stopifnot("You have to specify regex_on if match_by is set to 'regex'" = !is.null(regex_on))
    vars_exist(wt, vars = c("url"))
    stopifnot("couldn't find the column set in 'regex_on' in the classes data" = regex_on %in% names(classes))
    wt <- drop_query(wt)
    wt <- wt[, tmp_index := 1:.N]
    tmp_wt <- wt[, list(tmp_index, url_noquery)]
    pattern <- paste(classes[[regex_on]], collapse = "|")
    tmp_wt_matched <- tmp_wt[grepl(pattern, url_noquery)]
    tmp_wt_matched <- tmp_wt_matched[, match := regmatches(url_noquery, regexpr(pattern, url_noquery))]
    wt_matched <- tmp_wt_matched[, url_noquery := NULL][wt, on = "tmp_index"]
    setnames(wt_matched, "match", regex_on)
    wt <- classes[wt_matched, on = regex_on][, c("url_noquery", "tmp_index") := NULL]
  }

  if (!is.null(return_rows_by)) {
    vars_exist(classes, vars = return_rows_by)
    stopifnot("You have to specify return_rows_val if return_rows_by is not NULL" = !is.null(return_rows_val))
    wt <- wt[get(return_rows_by) == return_rows_val]
  }
  wt[]
}
