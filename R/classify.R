#' Classify visits by matching to a list of classes
#' @description
#' `classify_visits()` categorizes visits by either extracting the visit URL's
#' domain or host and matching them to a list of domains or hosts;
#' or by matching a list of regular expressions against the visit URL.
#' @param wt webtrack data object.
#' @param classes a data frame containing classes that can be matched to visits.
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
#' @return webtrack data.frame with the same columns as `wt` and any column
#' in `classes` except the column specified by `match_by`.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' data("domain_list")
#' wt <- as.wt_dt(testdt_tracking)
#' # classify visits via domain
#' wt_domains <- extract_domain(wt)
#' wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")
#' # classify visits via domain
#' # for the example, just renaming "domain" column
#' domain_list$host <- domain_list$domain
#' wt_hosts <- extract_host(wt)
#' wt_classes <- classify_visits(wt_hosts, classes = domain_list, match_by = "host")
#' # classify visits with pattern matching
#' # for the example, any value in "domain" treated as pattern
#' data("domain_list")
#' regex_list <- domain_list[type == "facebook"]
#' wt_classes <- classify_visits(wt[1:5000],
#'     classes = regex_list,
#'     match_by = "regex", regex_on = "domain"
#' )
#' # classify visits via domain and only return class "search"
#' data("domain_list")
#' wt_classes <- classify_visits(wt_domains,
#'     classes = domain_list,
#'     match_by = "domain", return_rows_by = "type",
#'     return_rows_val = "search"
#' )
#' }
#' @export
classify_visits <- function(wt, classes, match_by = "domain",
                            regex_on = NULL,
                            return_rows_by = NULL,
                            return_rows_val = NULL) {
    abort_if_not_wtdt(wt)
    match_by <- match.arg(match_by, c("domain", "host", "regex"))
    if (match_by == "domain") {
        vars_exist(wt, "domain")
        vars_exist(classes, "domain")
        wt <- merge(wt, classes, by = "domain", all.x = TRUE)
    } else if (match_by == "host") {
        vars_exist(wt, "host")
        vars_exist(classes, "host")
        wt <- merge(wt, classes, by = "host", all.x = TRUE)
    } else if (match_by == "regex") {
        stopifnot("You have to specify regex_on if match_by is set to 'regex'" = !is.null(regex_on))
        vars_exist(wt, "url")
        vars_exist(classes, regex_on)
        wt <- drop_query(wt)
        pattern <- paste(classes[[regex_on]], collapse = "|")
        idx <- grepl(pattern, wt$url_noquery)
        wt$match <- NA
        wt$match[idx] <- regmatches(wt$url_noquery, regexpr(pattern, wt$url_noquery))
        names(wt)[names(wt) == "match"] <- names(classes)[names(classes) != regex_on]
    }

    if (!is.null(return_rows_by)) {
        vars_exist(classes, return_rows_by)
        stopifnot("You have to specify return_rows_val if return_rows_by is not NULL" = !is.null(return_rows_val))
        wt <- wt[!is.na(wt[[return_rows_by]]) & wt[[return_rows_by]] == return_rows_val, ]
    }
    class(wt) <- c("wt_dt", class(wt))
    wt
}
