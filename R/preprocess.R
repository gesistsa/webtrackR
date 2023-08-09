#' Add time spent on a visit in seconds
#' @description
#' `add_duration()` approximates the time spent on a visit based on the difference
#' between two consecutive timestamps, replacing differences exceeding `cutoff` with
#' the value defined in `replace_by`.
#' @param wt webtrack data object.
#' @param cutoff numeric (seconds). If duration is greater than this value,
#' it is reset to the value defined by `replace_by`. Defaults to 300 seconds.
#' @param replace_by numeric. Determines whether differences greater than
#' the cutoff are set to `NA`, or some value. Defaults to `NA`.
#' @param last_replace_by numeric. Determines whether the last visit
#' for an individual is set to `NA`, or some value. Defaults to `NA`.
#' @param device_switch_na boolean. Relevant only when data was collected
#' from multiple devices. When visits are ordered by timestamp sequence,
#' two consecutive visits can come from different devices, which makes the
#' timestamp difference less likely to be the true duration. It may be
#' preferable to set the duration of the visit to `NA` (`TRUE`) rather than
#' the difference to the next timestamp (`FALSE`). Defaults to `FALSE`.
#' @param device_var character. Column indicating device.
#' Required if 'device_switch_na' set to `TRUE`. Defaults to `NULL`.
#' @importFrom data.table is.data.table shift setorder setnames
#' @return webtrack data.table (ordered by panelist_id and timestamp) with
#' the same columns as wt and a new column called `duration`.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt)
#' # Defining cutoff at 10 minutes, replacing those exceeding cutoff to 5 minutes,
#' # and setting duration before device switch to `NA`:
#' wt <- add_duration(wt, cutoff = 600, replace_by = 300,
#'                    device_switch_na = TRUE, device_var = "device")
#' @export
add_duration <- function(wt, cutoff = 300, replace_by = NA, last_replace_by = NA,
                         device_switch_na = FALSE, device_var = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  stopifnot("replace_by must be NA or greater/equal zero" = (is.na(replace_by) | replace_by > 0))
  if (device_switch_na == TRUE) {
    stopifnot("'device_var' must be specified if device_switch_na = TRUE" = !is.null(device_var))
  }
  vars_exist(wt, vars = c("panelist_id", "timestamp", device_var))
  setorder(wt, panelist_id, timestamp)
  wt[, duration := as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp), by = "panelist_id"]
  wt[, tmp_last := ifelse(is.na(duration), T, F)]
  if (device_switch_na == T) {
    setnames(wt, device_var, "device")
    wt[, device_next := shift(device, n = 1, type = "lead", fill = NA), by = "panelist_id"]
    wt[tmp_last == T, duration := last_replace_by]
    wt[duration > cutoff & tmp_last == F, duration := replace_by]
    wt[device_next != device & tmp_last == F, duration := NA]
    setnames(wt, "device", device_var)
    wt[, device_next := NULL]
  } else {
    wt[tmp_last == T, duration := last_replace_by]
    wt[duration > cutoff & tmp_last == F, duration := replace_by]
  }
  wt[, tmp_last := NULL]
  wt[]
}

#' Add a session variable
#' @description
#' `add_session()` groups visits into "sessions", defining a session to end
#' when the difference between two consecutive timestamps exceeds a `cutoff`.
#' @param wt webtrack data object.
#' @param cutoff numeric (seconds). If the difference between two consecutive
#' timestamps exceeds this value, a new browsing session is defined.
#' @importFrom data.table is.data.table shift setorder setnafill .N
#' @return webtrack data.table (ordered by panelist_id and timestamp)
#' with the same columns as wt and a new column called `session`.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Setting cutoff to 30 minutes
#' wt <- add_session(wt, cutoff = 1800)
#' @export
add_session <- function(wt, cutoff) {
  stopifnot("'cutoff' argument is missing" = !missing(cutoff))
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  wt[, tmp_index := 1:.N, by = panelist_id]
  wt[as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp) > cutoff, session := 1:.N, by = "panelist_id"]
  wt[, session := ifelse(tmp_index == 1, 1, session)]
  setnafill(wt, type = "locf", cols = "session")
  wt[, tmp_index := NULL]
  wt[]
}
#' Deduplicate visits
#' @description
#' `deduplicate()` flags, drops or aggregates duplicates, which are defined as
#' consecutive visits to the same URL within a certain time frame.
#' @param wt webtrack data object.
#' @param method character. One of `"aggregate"`, `"flag"` or `"drop"`.
#' If set to `"aggregate"`, consecutive visits (no matter the time difference)
#' to the same URL are combined and their duration aggregated.
#' In this case, a duration column must be specified via `"duration_var"`.
#' If set to `"flag"`, duplicates within a certain time frame are flagged in a new
#' column called `duplicate`. In this case, `within` argument must be specified.
#' If set to `"drop"`, duplicates are dropped. Again, `within` argument must be specified.
#' Defaults to `"aggregate"`.
#' @param within numeric (seconds). If `method` set to `"flag"` or `"drop"`,
#' a subsequent visit is only defined as a duplicate when happening within
#' this time difference. Defaults to 1 second.
#' @param duration_var character. Name of duration variable. Defaults to `"duration"`.
#' @param keep_nvisits boolean. If method set to `"aggregate"`, this determines whether
#' number of aggregated visits should be kept as variable. Defaults to `FALSE`.
#' @param same_day boolean. If method set to `"aggregate"`, determines
#' whether to count visits as consecutive only when on the same day. Defaults to `TRUE`.
#' @importFrom data.table is.data.table shift .N setnames setorder
#' @return webtrack data.table with the same columns as wt with updated duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt, cutoff = 300, replace_by = 300)
#' # Dropping duplicates with one-second default
#' wt_dedup <- deduplicate(wt, method = "drop")
#' # Flagging duplicates with one-second default
#' wt_dedup <- deduplicate(wt, method = "flag")
#' # Aggregating duplicates
#' wt_dedup <- deduplicate(wt[1:1000], method = "aggregate")
#' # Aggregating duplicates and keeping number of visits for aggregated visits
#' wt_dedup <- deduplicate(wt[1:1000], method = "aggregate", keep_nvisits = TRUE)
#' @export
deduplicate <- function(wt, method = "aggregate", within = 1, duration_var = "duration",
                        keep_nvisits = FALSE, same_day = TRUE) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  if (method == "aggregate") {
    vars_exist(wt, vars = duration_var)
    setnames(wt, duration_var, "duration")
    wt[, visit := cumsum(url != shift(url, n = 1, type = "lag", fill = 0)), by = "panelist_id"]
    if (same_day == TRUE) {
      wt[, day := as.Date(timestamp)]
      grp_vars <- c("panelist_id", "visit", "url", "day")
      wt <- wt[, list(
        visits = .N,
        duration = sum(as.numeric(duration), na.rm = TRUE),
        timestamp = min(timestamp)
      ), by = grp_vars]
      wt[, day := NULL]
    } else {
      grp_vars <- c("panelist_id", "visit", "url")
      wt <- wt[, list(
        visits = .N,
        duration = sum(as.numeric(duration), na.rm = TRUE),
        timestamp = min(timestamp)
      ), by = grp_vars]
    }
    wt[, visit := NULL]
    if (keep_nvisits == FALSE) {
      wt[, visits := NULL]
    }
    setnames(wt, "duration", duration_var)
  } else if (method %in% c("drop", "flag")) {
    stopifnot("'within' must be specified if 'method' set to 'flag' or 'drop" = !is.null(within))
    wt[, tmp_timestamp_prev := shift(timestamp, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    wt[, tmp_url_prev := shift(url, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    wt[, duplicate := ifelse(is.na(tmp_url_prev), FALSE, ifelse(
      (timestamp - tmp_timestamp_prev <= within) & (url == tmp_url_prev), TRUE, FALSE
    )),
    by = "panelist_id"
    ]
    if (method == "drop") {
      wt <- wt[duplicate == FALSE]
      wt[, duplicate := NULL]
    }
    wt[, tmp_url_prev := NULL]
    wt[, tmp_timestamp_prev := NULL]
  }
  wt[]
}

#' Extract the host from URL
#' @description
#' `extract_host()` adds the host of a URL as a new column.
#' The host is defined as the part following the scheme (e.g., "https://") and
#' preceding the subdirectory (anything following the next "/").
#' @param wt webtrack data object.
#' @param varname character. Name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @param drop_na boolean. Determines whether rows for which no host can be extracted
#' should be dropped from the data. Defaults to `TRUE`.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt
#' and a new column called `'host'` (or, if varname not equal to `'url'`, `'<varname>_host'`)
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract host and drop rows without host
#' wt <- extract_host(wt)
#' # Extract host and keep rows without host
#' wt <- extract_host(wt, drop_na = FALSE)
#' @export
extract_host <- function(wt, varname = "url", drop_na = TRUE) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  if (varname == "url") {
    wt[, host := urltools::domain(tmp_host)]
    n_na <- nrow(wt[is.na(host)])
    if (drop_na == TRUE) {
      wt <- wt[!is.na(host)]
      if (n_na > 0) {
        warning(paste0("Host could not be extracted for ", n_na, " rows, which were dropped from the data. Set drop_na = FALSE to keep these rows."))
      }
    } else {
      if (n_na > 0) {
        warning(paste0("Host could not be extracted for ", n_na, " rows. Set drop_na = TRUE to drop these rows."))
      }
    }
  } else {
    wt[, paste0(varname, "_host") := urltools::domain(tmp_host)]
    n_na <- nrow(wt[is.na(paste0(varname, "_host"))])
    if (drop_na == TRUE) {
      wt <- wt[!is.na(paste0(varname, "_host"))]
      if (n_na > 0) {
        warning(paste0("Host could not be extracted for ", n_na, " rows, which were dropped from the data. Set drop_na = FALSE to keep these rows."))
      }
    } else {
      if (n_na > 0) {
        warning(paste0("Host could not be extracted for ", n_na, " rows. Set drop_na = TRUE to drop these rows."))
      }
    }
  }
  wt[, tmp_host := NULL]
  wt[]
}

#' Extract the domain from URL
#' @description
#' `extract_domain()` adds the domain of a URL as a new column.
#' By "domain", we mean the "top private domain", i.e., the domain under
#' the public suffix (e.g., "`com`") as defined by the Public Suffix List.
#' See details.
#' @details
#' We define a "web domain" in the common colloquial meaning, that is,
#' the part of an web address that identifies the person or organization in control.
#' is `google.com`. More technically, what we mean by "domain" is the
#' "top private domain", i.e., the domain under the public suffix,
#' as defined by the Public Suffix List.
#' Note that this definition sometimes leads to counterintuitive results because
#' not all public suffixes are "registry suffixes". That is, they are not controlled
#' by a domain name registrar, but allow users to directly register a domain.
#' One example of such a public, non-registry suffix is `blogspot.com`. For a URL like
#' `www.mysite.blogspot.com`, our function, and indeed the packages we are aware of,
#' would extract the domain as `mysite.blogspot.com`, although you might think of
#' `blogspot.com` as the domain.
#' For details, see \link{https://github.com/google/guava/wiki/InternetDomainNameExplained}.
#' @param wt webtrack data object.
#' @param varname character. Name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @param drop_na boolean. Determines whether rows for which no host can be extracted
#' should be dropped from the data. Defaults to `TRUE`.
#' @description Extracts the domain from urls.
#' @importFrom data.table is.data.table fcase
#' @return webtrack data.table with the same columns as wt
#' and a new column called `'domain'`
#'(or, if varname not equal to `'url'`, `'<varname>_domain'`)
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract domain and drop rows without domain
#' wt <- extract_domain(wt)
#' # Extract domain and keep rows without domain
#' wt <- extract_domain(wt, drop_na = FALSE)
#' @export
extract_domain <- function(wt, varname = "url", drop_na = TRUE) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  wt[, tmp_suffix := urltools::suffix_extract(tmp_host)[["suffix"]]]
  wt[, tmp_domain_name := urltools::suffix_extract(tmp_host)[["domain"]]]
  if (varname == "url") {
    wt[, domain := ifelse((!is.na(tmp_suffix)), paste0(tmp_domain_name, ".", tmp_suffix), NA)]
  } else {
    wt[, paste0(varname, "_domain") := ifelse((!is.na(tmp_suffix)), paste0(tmp_domain_name, ".", tmp_suffix), NA)]
  }
  if (varname == "url") {
    wt[, domain := fcase(
      is.na(tmp_suffix), NA_character_,
      !is.na(tmp_suffix) & is.na(tmp_domain_name), tmp_suffix,
      !is.na(tmp_suffix) & !is.na(tmp_domain_name), paste0(tmp_domain_name, ".", tmp_suffix))]
    n_na <- nrow(wt[is.na(domain)])
  } else {
    wt[, paste0(varname, "_domain") := fcase(
      is.na(tmp_suffix), NA_character_,
      !is.na(tmp_suffix) & is.na(tmp_domain_name), tmp_suffix,
      !is.na(tmp_suffix) & !is.na(tmp_domain_name), paste0(tmp_domain_name, ".", tmp_suffix))]
    n_na <- nrow(wt[is.na(paste0(varname, "_domain"))])
  }
  wt[, tmp_host := NULL]
  wt[, tmp_suffix := NULL]
  wt[, tmp_domain_name := NULL]
  if (drop_na == TRUE) {
    if (varname == "url") {
      wt <- wt[!is.na(domain)]
    } else {
      wt <- wt[!is.na(paste0(varname, "_domain"))]
    }
    if (n_na > 0) {
      warning(paste0("Domain could not be extracted for ", n_na, " rows, which were dropped from the data. Set drop_na = FALSE to keep these rows."))
    }
  } else {
    if (n_na > 0) {
      warning(paste0("Domain could not be extracted for ", n_na, " rows. Set drop_na = TRUE to drop these rows."))
    }
  }
  wt[]
}

#' Extract the path from URL
#' @description
#' `extract_path()` adds the path of a URL as a new column.
#' The path is defined as the part following the host but not including a
#' query (anything after a "?") or a fragment (anything after a "#").
#' @param wt webtrack data object
#' @param varname character. name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt
#' and a new column called `'path'` (or, if varname not equal to `'url'`, `'<varname>_path'`)
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract path
#' wt <- extract_path(wt)
#' @export
extract_path <- function(wt, varname = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  wt[, tmp_path := urltools::path(gsub("@", "%40", get(varname)))]
  if (varname == "url") {
    wt[, path := gsub("%40", "@", tmp_path)]
  } else {
    wt[, paste0(varname, "_path") := gsub("%40", "@", tmp_path)]
  }
  wt[, tmp_host := NULL]
  wt[, tmp_path := NULL]
  wt[]
}

#' Drop the query and fragment from URL
#' #' @description
#' `drop_query()` adds the URL without query and fragment as a new column.
#' The query is defined as the part following a "?" after the path.
#' The fragement is anything following a "#" after the query.
#' @param wt webtrack data object.
#' @param varname character. name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @importFrom data.table is.data.table %like%
#' @return webtrack data.table with the same columns as wt
#' and a new column called `'<varname>_noquery'`
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract URL without query/fragment
#' wt <- drop_query(wt)
#' @export
drop_query <- function(wt, varname = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  wt[, tmp_path := urltools::path(gsub("@", "%40", get(varname)))]
  wt[, tmp_path := gsub("%40", "@", tmp_path)]
  wt[, tmp_scheme := urltools::scheme(get(varname))]
  wt[is.na(tmp_host), tmp_host := ""]
  wt[is.na(tmp_path), tmp_path := ""]
  wt[is.na(tmp_scheme), tmp_scheme := ""]
  wt[, paste0(varname, "_noquery") := paste0(tmp_scheme, "://", tmp_host, "/", tmp_path, recycle0 = T)]
  wt[, tmp_host := NULL]
  wt[, tmp_path := NULL]
  wt[, tmp_scheme := NULL]
  wt[]
}

#' Add the next visit as a new column
#' #' @description
#' `add_next_visit()` adds the subsequent visit, as determined by order of
#' timestamps as a new column. The next visit can be added as either the full URL,
#' the extracted host or the extracted domain, depending on `level`.
#' @param wt webtrack data object.
#' @param level character. Either `"url"`, `"host"` or `"domain"`. Defaults to `"url"`.
#' @importFrom data.table is.data.table shift setorder
#' @return webtrack data.table with the same columns as wt and
#' a new column called `url_next`,`host_next` or `domain_next`.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Adding next full URL as new column
#' wt <- add_next_visit(wt, level = "url")
#' # Adding next host as new column
#' wt <- add_next_visit(wt, level = "host")
#' # Adding next domain as new column
#' wt <- add_next_visit(wt, level = "domain")
#' @export
add_next_visit <- function(wt, level = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  if (level == "url") {
    wt[, url_next := shift(url, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  } else if (level == "host") {
    if (!"host" %in% names(wt)) {
      suppressWarnings(wt <- extract_host(wt, varname = "url", drop_na = F))
      wt[, host_next := shift(host, n = 1, type = "lead", fill = NA), by = "panelist_id"]
      wt[, host := NULL]
    } else {
      wt[, host_next := shift(host, n = 1, type = "lead", fill = NA), by = "panelist_id"]
    }
  } else if (level == "domain") {
    if (!"domain" %in% names(wt)) {
      suppressWarnings(wt <- extract_domain(wt, varname = "url", drop_na = F))
      wt[, domain_next := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
      wt[, domain := NULL]
    } else {
      wt[, domain_next := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    }
  }
  wt[]
}

#' Add the previous visit as a new column
#' #' @description
#' `add_previous_visit()` adds the previous visit, as determined by order of
#' timestamps as a new column The previous visit can be added as either the full URL,
#' the extracted host or the extracted domain, depending on `level`.
#' @param wt webtrack data object.
#' @param level character. Either `"url"`, `"host"` or `"domain"`. Defaults to `"url"`.
#' @importFrom data.table is.data.table shift setorder
#' @return webtrack data.table with the same columns as wt and
#' a new column called `url_previous`,`host_previous` or `domain_previous.`.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Adding previous full URL as new column
#' wt <- add_previous_visit(wt, level = "url")
#' # Adding previous host as new column
#' wt <- add_previous_visit(wt, level = "host")
#' # Adding previous domain as new column
#' wt <- add_previous_visit(wt, level = "domain")
#' @export
add_previous_visit <- function(wt, level = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  if (level == "url") {
    wt[, url_previous := shift(url, n = 1, type = "lag", fill = NA), by = "panelist_id"]
  } else if (level == "host") {
    if (!"host" %in% names(wt)) {
      suppressWarnings(wt <- extract_host(wt, varname = "url", drop_na = F))
      wt[, host_previous := shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
      wt[, host := NULL]
    } else {
      wt[, host_previous := shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    }
  } else if (level == "domain") {
    if (!"domain" %in% names(wt)) {
      suppressWarnings(wt <- extract_domain(wt, varname = "url", drop_na = F))
      wt[, domain_previous := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
      wt[, domain := NULL]
    } else {
      wt[, domain_previous := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    }
  }
  wt[]
}

#' Download and add the "title" of a URL
#' @description
#' `add_title()` gets the title of a URL by accessing the web address online
#' and adds the title as a new column. See details for the meaning of "title".
#' You need an internet connection to run this function.
#' @details The title of a website (the text within the `<title>` tag
#' of a web site's `<head>`) #' is the text that is shown on the "tab"
#' when looking at the website in a browser. It can contain useful information
#' about a URL's content and can be used, for example, for classification purposes.
#' Note that it may take a while to run this function for a large number of URLs.
#' @param wt webtrack data object.
#' @param lang character (a language tag). Language accepted by the request.
#' Defaults to `"en-US,en-GB,en"`. Note that you are likely to still obtain titles
#' different from the ones seen originally by the user, because the language
#' also depend on the user's IP and device settings.
#' @importFrom data.table is.data.table
#' @importFrom rvest html_text html_node read_html
#' @importFrom httr GET add_headers
#' @return webtrack data.table with the same columns as wt and a new column
#' called `"title"`, which will be `NA` if the title cannot be retrieved.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)[1:10]
#' # Get titles with `lang` set to default English
#' wt_titles <- add_title(wt)
#' # Get titles with `lang` set to German
#' wt_titles <- add_title(wt, lang = "de")
#' }
#' @export
add_title <- function(wt, lang = "en-US,en-GB,en") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "url"))
  urls <- data.table(url = unique(wt$url))
  urls[, title := mapply(function(x) {
    return(
      tryCatch(
        html_text(html_node(
          read_html(
            GET(x, add_headers(.headers = c(
              "user_agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
              "Accept-language" = lang
            )))
          ),
          "head title"
        )),
        error = function(e) NA
      )
    )
  }, url)]
  closeAllConnections()
  wt <- wt[urls, on = "url"]
  wt[]
}

#' Add social media referrals as a new column
#' @description
#' Identifies whether a visit was referred to from social media and
#' adds it as a new column. See details for method.
#' @details To identify referrals, we rely on the method described as most valid
#' in Schmidt et al.: When the domain preceding a visit was to the platform in question,
#' and the query string of the visit's URL contains a certain pattern,
#' we count it as a referred visit. For Facebook, the pattern has been identified
#' by Schmidt et al. as `'fbclid='`, although this can change in future.
#' @param wt webtrack data object.
#' @param platform_domains character. A vector of platform domains for which
#' referrers should be identified. Order and length must correspondent to `patterns` argument
#' @param patterns character. A vector of patterns for which referrers should
#' be identified. Order and length must correspondent to `platform_domains` vector.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called `referral`,
#' which takes on NA if no referral has been identified, or the name specified
#' platform_domains if a referral from that platform has been identified
#' @references
#' Schmidt, Felix, Frank Mangold, Sebastian Stier and Roberto Ulloa. "Facebook as an Avenue to News: A Comparison and Validation of Approaches to Identify Facebook Referrals". Working paper.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
#' wt <- add_referral(wt,
#'   platform_domains = c("facebook.com", "twitter.com"),
#'   patterns = c("fbclid=", "utm_source=twitter")
#' )
#' @export
add_referral <- function(wt, platform_domains, patterns) {
  stopifnot("Input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "url", "timestamp"))
  stopifnot("Number of platform_domains must be identical to number of patterns" = length(platform_domains) == length(patterns))
  wt <- add_previous_visit(wt, level = "domain")
  wt[, referral := NA]
  for (i in seq_along(platform_domains)) {
    wt[, referral := ifelse(grepl(patterns[i], url) &
      domain_previous == platform_domains[i] &
      is.na(referral), platform_domains[i], referral)]
  }
  wt[, domain_previous := NULL]
  wt[]
}

#' Create an urldummy variable from a data.table object
#' @param wt webtrack data object
#' @param dummy a vector of urls that should be dummy coded
#' @param name name of dummy variable to create.
#' @importFrom data.table setnames setattr
#' @return webtrack object with the same columns and a new column called "name" including the dummy variable
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
#' code_urls <- "https://dkr1.ssisurveys.com/tzktsxomta"
#' create_urldummy(wt, dummy = code_urls, name = "test_dummy")
#' @export
create_urldummy <- function(wt, dummy, name) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("url"))
  wt[, dummy := data.table::fifelse(url %in% dummy, TRUE, FALSE)]
  setnames(wt, "dummy", name)
  setattr(wt, "dummy", c(attr(wt, "dummy"), name))
  wt[]
}

#' Add panelist features to tracking data
#' @description
#' `add_panelist_data()` adds information about panelists (e.g., from a survey)
#' to the tracking data.
#' @param wt webtrack data object.
#' @param data a data.table (or object that can be converted to data.table)
#'  which contains columns about panelists
#' @param cols character vector of columns to add. If `NULL`, all columns are added.
#' Defaults to `NULL`.
#' @param join_on which columns to join on. Defaults to `"panelist_id"`.
#' @importFrom data.table is.data.table as.data.table setattr
#' @return webtrack object with the same columns and the columns from `data`
#' specified in `cols`.
#' @examples
#' data("testdt_tracking")
#' data("testdt_survey_w")
#' wt <- as.wt_dt(testdt_tracking)
#' # add survey test data
#' add_panelist_data(wt, testdt_survey_w)
#' @export
add_panelist_data <- function(wt, data, cols = NULL, join_on = "panelist_id") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c(join_on))
  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }
  vars_exist(data, vars = c(join_on))
  if (!is.null(cols)) {
    if (!all(cols %in% names(data))) {
      stop("couldn't locate all columns in data")
    }
    data <- data[, c(join_on, cols), with = FALSE]
    setattr(wt, "panelist", cols)
  } else {
    setattr(wt, "panelist", setdiff(names(data), join_on))
  }
  data[wt, on = join_on]
}
