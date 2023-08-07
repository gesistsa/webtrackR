#' Add time spent on a visit in seconds
#' @description
#' `add_duration()` approximates the time spent on a visit based on the difference
#' between two consecutive timestamps, replacing differences exceeding `cutoff` with
#' the value defined in `replace_by`.
#' @param wt webtrack data object
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
#' the same columns as wt and a new column called `duration`
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

#' Create a session variable
#' @description Define sessions of browsing depending on aq time cutoff for inactivity
#' @param wt webtrack data object
#' @param cutoff numeric. If the consecutive visit happens later than this value (in seconds), a new browsing session is defined
#' @importFrom data.table is.data.table shift setorder setnafill
#' @return webtrack data.table (ordered by panelist_id and timestamp) with the same columns as wt and a new column called duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_session(wt, cutoff = 1800)
#' @export
add_session <- function(wt, cutoff) {
  stopifnot("cutoff argument is missing" = !missing(cutoff))
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  wt[as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp) > cutoff, session := 1:.N, by = "panelist_id"]
  setnafill(wt, type = "nocb", cols = "session")
  wt[]
}
#' Deduplicate visits
#' @description Deduplicate visits or aggregate the duration of duplicate visits
#' @details
#' A duplicate visit is defined as a visit to the same URL as the previous visit,
#' within a certain time frame.
#' @param wt webtrack data object
#' @param method character. "aggregate", "flag" or "drop". If set to "aggregate",
#' consecutive visits to the same URL are combined and their duration aggregated.
#' In this case, a duration column must be specified via "duration_var";
#' if "within" is specified, only
#' If set to "flag", duplicates within a certain time frame are flagged in a new
#' column called "duplicate". In this case, "within" argument must be specified.
#' If set to "drop", duplicates are dropped. Again, "within" argument must be specified.
#' Defaults to "aggregate".
#' @param within numeric. If method set to "flag or "drop",
#' a duplicate is defined only when the consecutive visit happens within
#' this time difference (in seconds). Defaults to 1.
#' @param duration_var character. Name of duration variable. Defaults to "duration".
#' @param keep_nvisits logical. If method set to "aggregate", this determines whether
#' number of aggregated visits should be kept as variable. Defaults to FALSE.
#' @param same_day logical. If method set to "aggregate", determines
#' whether to count visits as consecutive only when on the same day. Defaults to TRUE.
#' @importFrom data.table is.data.table shift .N setnames setorder
#' @return webtrack data.table with the same columns as wt with updated duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt, cutoff = 300, replace_by = 300)
#' deduplicate(wt, method = "drop")
#' # deduplicate(wt, method = "aggregate", keep_nvisits = TRUE)
#' @export
deduplicate <- function(wt, method = "flag", within = 1, duration_var = NULL,
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

#' Extract host from url
#' @description Extracts the host from urls. The host is defined as the part between the scheme (e.g., "https://") and the subdirectory.
#' @param wt webtrack data object
#' @param varname character. name of the URL variable from which to extract the host. Defaults to "url".
#' @param drop_na boolean. Whether rows for which no host can be extracted should be dropped from the data. Defaults to TRUE.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called 'host' (or, if varname not equal to 'url', '<varname>_host')
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_host(wt)
#' @export
extract_host <- function(wt, varname = "url", drop_na = TRUE) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  if (varname == "url") {
    wt[, host := urltools::domain(tmp_host)]
  } else {
    wt[, paste0(varname, "_host") := urltools::domain(tmp_host)]
  }
  wt[, tmp_host := NULL]
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
  wt[]
}

#' Extract domain from url
#' @description Extracts the domain from urls. We define the domain (e.g., "google.com") as the sum of the suffix (e.g., ".com") and the part before that and the preceding dot (e.g., "google" in "https://mail.google.com).
#' @param wt webtrack data object
#' @param varname name of the URL variable from which to extract the domain Defaults to "url".
#' @param drop_na boolean. Whether rows for which no domain can be extracted should be dropped from the data. Defaults to TRUE.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called 'domain' (or, if varname not equal to 'url', '<varname>_domain')
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
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
  wt[, tmp_host := NULL]
  wt[, tmp_suffix := NULL]
  wt[, tmp_domain_name := NULL]
  n_na <- nrow(wt[is.na(domain)])
  if (drop_na == TRUE) {
    wt <- wt[!is.na(domain)]
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

#' Extract path from url
#' @description Extracts the path from urls.
#' @param wt webtrack data object
#' @param varname name of the URL variable from which to extract the domain Defaults to "url".
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called 'path' (or, if varname not equal to 'url', '<varname>_path')
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_path(wt)
#' @export
extract_path <- function(wt, varname = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = varname)
  wt[, tmp_host := urltools::domain(gsub("@", "%40", get(varname)))]
  wt[, tmp_path := urltools::path(gsub("@", "%40", get(varname)))]
  wt[, path := gsub("%40", "@", tmp_path)]
  wt[, tmp_host := NULL]
  wt[, tmp_path := NULL]
  wt[]
}

#' Drop query/fragment from URL
#' @description Drops the query and fragment from a URL, i.e., everything after a "?" or "#"
#' @param wt webtrack data object
#' @param varname name of the URL variable from which to drop the query/fragment. Defaults to "url".
#' @param drop_na boolean. Whether rows for which the URL cannot be split into its components should be dropped from the data. Defaults to TRUE.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called "<varname>_noquery"
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- drop_query(wt)
#' @export
drop_query <- function(wt, varname = "url", drop_na = TRUE) {
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

#' Add the next visit as a variable
#' @description Adds the next visit as a variable, either as the full url, the extracted host or the extracted domain
#' @param wt webtrack data object
#' @param level character. Either "url", "host" or "domain". Defaults to "url".
#' @importFrom data.table is.data.table shift setorder
#' @return webtrack data.table (ordered by panelist_id and timestamp) with the same columns as wt and a new column called url_next, host_next or domain_next.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_next_visit(wt, level = "url")
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

#' Add the previous visit as a variable
#' @description Adds the previous visit as a variable, either as the full url, the extracted host or the extracted domain
#' @param wt webtrack data object
#' @param level character. Either "url", "host" or "domain". Defaults to "url".
#' @importFrom data.table is.data.table shift setorder
#' @return webtrack data.table (ordered by panelist_id and timestamp) with the same columns as wt and a new column called url_previous, host_previous or domain_previous.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_previous_visit(wt, level = "url")
#' @export
add_previous_visit <- function(wt, level = "url") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  if (level == "url") {
    wt[, url_previous := shift(url, n = 1, type = "lag", fill = NA), by = "panelist_id"]
  } else if (level == "host") {
    if (!"host" %in% names(wt)) {
      wt <- extract_host(wt, varname = "url", drop_na = F)
      wt[, host_previous := shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
      wt[, host := NULL]
    } else {
      wt[, host_previous := shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    }
  } else if (level == "domain") {
    if (!"domain" %in% names(wt)) {
      wt <- extract_domain(wt, varname = "url", drop_na = F)
      wt[, domain_previous := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
      wt[, domain := NULL]
    } else {
      wt[, domain_previous := shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    }
  }
  wt[]
}

#' Download and add the "title" of a URL
#' @description Gets the title of a website and adds it as a new variable. See details.
#' @details The title of a website (the text within the <title> tag of a web sites <head>)
#' is the text that is shown onn the "tab" when looking at the website in a browser.
#' It can contain useful information about a URL's content and can be used, for example,
#' for classification purposes. Note that it may take a while to run this function for a large number of URLs.
#' @param wt webtrack data object
#' @param lang character (a language tag). Language accepted by the request. Defaults to "en-US,en-GB,en".
#' Note that you are likely to still obtain titles different from the ones seen originally by the user,
#' because the language also depend on the user's IP and device settings.
#' @importFrom data.table is.data.table
#' @importFrom rvest html_text html_node read_html
#' @importFrom httr GET add_headers
#' @return webtrack data.table with the same columns as wt and a new column called "title",
#' which will be NA if the title cannot be retrieved.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)[1:10]
#' wt_titles <- add_title(wt)
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

#' Identify social media referrals
#' @description Identifies whether a web visit was referred to from social media. See details.
#' @details To identify referrals, we rely on the method described as most valid in Schmidt et al.:
#' When the domain preceding a visit was to the platform in question,
#' and the query string of the visit's URL contains a certain pattern, we count it as a referred visit.
#' For Facebook, the pattern has been identified by Schmidt et al. as 'fbclid=', although this can change in future.
#' @param wt webtrack data object
#' @param platform_domains character. A vector of platform domains for which referrers should be identified. Order and length must correspondent to 'patterns' vector.
#' @param patterns character. A vector of patterns for which referrers should be identified. Order and length must correspondent to 'platform_domains' vector.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called 'referral'.
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



#' Add panelist features to webtrack data
#' Add characteristics of panelists (e.g. from a survey) to the webtrack data
#' @param wt webtrack data object
#' @param data a data.table (or object that can be converted to data.table) which contains variables of panelists
#' @param cols character vector of columns to add. If NULL, all columns are added
#' @param join_on which columns to join on. Defaults to "panelist_id".
#' @return webtrack object with the same columns and joined with panelist survey data
#' @examples
#' data("testdt_tracking")
#' data("testdt_survey_w")
#' wt <- as.wt_dt(testdt_tracking)
#' add_panelist_data(wt, testdt_survey_w)
#' @export
add_panelist_data <- function(wt, data, cols = NULL, join_on = "panelist_id") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c(join_on))
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  vars_exist(data, vars = c(join_on))
  if (!is.null(cols)) {
    if (!all(cols %in% names(data))) {
      stop("couldn't locate all cols in data")
    }
    data <- data[, c(join_on, cols), with = FALSE]
    data.table::setattr(wt, "panelist", cols)
  } else {
    data.table::setattr(wt, "panelist", setdiff(names(data), join_on))
  }
  data[wt, on = join_on]
}
