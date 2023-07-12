#' Classify visits by matching to a list of classes
#' @param wt webtrack data object
#' @param classes a data.table containing classes that can be matched to visits.
#' @param match_by character. Whether to match list entry to the domain of a visit ("domain") or the host ("host") with an exact match;
#' or with a regular expression against the whole URL of a visit. If set to "domain" or "host",
#' both "wt" and "classes" need to have a column called thus. If set to "regex",
#' the "url" column of "wt" will be used, and you need to set "regex_on" to the
#' column in "classes" for which to do the regex matching. Defaults to "domain".
#' @param regex_on character. Column in "classes" on which to do the regex matching. Defaults to NULL.
#' @param return_rows_by character. A column in "classes" on which to subset the joined data.
#' @param return_rows_val character. The value of the columns specified in "return_rows_by", for which
#' data should be returned.
#' @importFrom data.table is.data.table setnames
#' @return webtrack data.table with the same columns as wt and any column in classes except the "match_by" column.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' data("domain_list")
#' wt <- as.wt_dt(testdt_tracking)
#' wt_domains <- extract_domain(wt, drop_na = F)
#' wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")
#' domain_list$host <- domain_list$domain
#' wt_hosts <- extract_host(wt, drop_na = F)
#' wt_classes <- classify_visits(wt_hosts, classes = domain_list, match_by = "host")
#' data("domain_list")
#' regex_list = domain_list[type == "newsportals"]
#' wt_classes <- classify_visits(wt, classes = regex_list, match_by = "regex", regex_on = "domain")
#' }
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
  } else if (match_by == "regex")  {
    stopifnot("You have to specify regex_on if match_by is set to 'regex'" = !is.null(regex_on))
    vars_exist(wt, vars = c("url"))
    stopifnot("couldn't find the column set in regex_on in the classes data" = regex_on %in% names(classes))
    wt <- drop_query(wt)
    wt <- wt[,tmp_index := 1:.N]
    tmp_wt <- wt[,.(tmp_index, url_noquery)]
    pattern <- paste(classes[[regex_on]], collapse = "|")
    tmp_wt_matched <- tmp_wt[grepl(pattern, url_noquery)]
    tmp_wt_matched <- tmp_wt_matched[, match := regmatches(url_noquery, regexpr(pattern, url_noquery))]
    wt_matched <- tmp_wt_matched[,url_noquery:=NULL][wt, on = "tmp_index"]
    setnames(wt_matched, "match", regex_on) # this is still hacky
    wt <- classes[wt_matched, on = regex_on][,c("url_noquery", "tmp_index"):=NULL]
  }

  if (!is.null(return_rows_by)) {
    vars_exist(classes, vars = return_rows_by)
    stopifnot("You have to specify return_rows_val if return_rows_by is not NULL" = !is.null(return_rows_val))
    wt <- wt[get(return_rows_by) == return_rows_val]
  }
  wt[]
}

#' Classify domains according to prespecified classes
#' @param wt webtrack data object
#' @param domain_classes a data.table containing a column "domain" and "type". If NULL, an internal list is used
#' @param prev_type logical. If TRUE (default) the type of the domain visited before the current visit is added
#' @param preprocess_newsportals logical. add suffix "NEWS" to domains which are classified as portals. If TRUE there needs to be a domain type "newsportals"
#' @param return.only if not null, only return the specified domain type
#' @return webtrack data.table with the same columns as wt and a new column called type. If prev_type is TRUE, a column prev_type is added with the type of the visit before the current one. If newsportals are processed, found newsportals have an added "/NEWS" in the domain column. If return.only is used, only rows that contain a specific domain type are returned
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
#' wt <- add_duration(wt)
#' wt <- classify_domains(wt)
#' }
#' @export
classify_domains <- function(wt,
                             domain_classes = NULL,
                             prev_type = TRUE,
                             preprocess_newsportals = FALSE,
                             return.only = NULL) {
  # i.type <- NULL # revisit

  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("url", "domain"))

  if (is.null(domain_classes)) {
    domain_classes <- webtrackR::domain_list
  }

  if (!data.table::is.data.table(domain_classes)) {
    stop("domain_classes needs to be a data.table")
  }

  if (!all(c("domain", "type") %in% names(domain_classes))) {
    stop("domain_classes must contain columns called 'domain' and 'type'")
  }

  if (!is.null(return.only)) {
    stopifnot("return.only not found in domain_classes" = !return.only %in% domain_classes[["type"]])
  }

  if (isTRUE(preprocess_newsportals)) {
    if (!"newsportals" %in% domain_classes[["type"]]) {
      warning("newsportals type is missing in domain_classes. No preprocessing done")
    } else {
      doms <- paste(domain_classes[["domain"]][domain_classes[["type"]] == "newsportals"], collapse = "|")
      nportal_idx <- grepl(doms, url, perl = TRUE)
      wt[, domain := data.table::fifelse(nportal_idx, paste0(domain, "/NEWS"), domain)]
    }
  }
  wt[domain_classes, on = "domain", type := i.type]
  wt[is.na(type), type := "other"]

  if (preprocess_newsportals) {
    wt[nportal_idx, type := "news"]
  }
  if (!is.null(return.only)) {
    wt <- wt[type == return.only]
  }
  if (isTRUE(prev_type)) {
    wt[, day := as.Date(timestamp)]
    wt[, prev_type := data.table::shift(type, n = 1L, fill = "other"), by = c("panelist_id", "day")]
    wt[, prev_type := data.table::fifelse(data.table::shift(duration, n = 1L, fill = 5000) > 3600, "direct", prev_type)]
    wt[, day := NULL]
  }
  wt[]
}

