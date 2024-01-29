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
#' @importFrom data.table := .N .SD
#' @return webtrack data.frame with
#' the same columns as wt and a new column called for duration.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt)
#' # Defining cutoff at 10 minutes, replacing those exceeding cutoff to 5 minutes,
#' # and setting duration before device switch to `NA`:
#' wt <- add_duration(wt,
#'     cutoff = 600, replace_by = 300,
#'     device_switch_na = TRUE, device_var = "device"
#' )
#' }
#' @export

add_duration <- function(wt, cutoff = 300, replace_by = NA, last_replace_by = NA,
                         device_switch_na = FALSE, device_var = NULL) {
    abort_if_not_wtdt(wt)
    stopifnot((is.na(replace_by) | replace_by >= 0))
    if (device_switch_na == TRUE) {
        stopifnot(!is.null(device_var))
    }

    required_vars <- c("panelist_id", "timestamp")
    if (!is.null(device_var)) required_vars <- c(required_vars, device_var)
    missing_vars <- setdiff(required_vars, names(wt))
    if (length(missing_vars) > 0) {
        stop(paste("Missing required columns:", paste(missing_vars, collapse = ", ")))
    }

    data.table::setorder(wt, panelist_id, timestamp)
    wt[, duration := as.numeric(data.table::shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp), by = "panelist_id"]
    wt[, tmp_last := ifelse(is.na(duration), TRUE, FALSE)]
    if (device_switch_na == TRUE) {
        data.table::setnames(wt, device_var, "device")
        wt[, device_next := data.table::shift(device, n = 1, type = "lead", fill = NA), by = "panelist_id"]
        wt[tmp_last == TRUE, duration := last_replace_by]
        wt[duration > cutoff & tmp_last == FALSE, duration := replace_by]
        wt[device_next != device & tmp_last == FALSE, duration := NA]
        data.table::setnames(wt, "device", device_var)
        wt[, device_next := NULL]
    } else {
        wt[tmp_last == TRUE, duration := last_replace_by]
        wt[duration > cutoff & tmp_last == FALSE, duration := replace_by]
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
#' @return webtrack data.frame with the same columns as wt and a new column called session.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Setting cutoff to 30 minutes
#' wt <- add_session(wt, cutoff = 1800)
#' }
#' @export
add_session <- function(wt, cutoff) {
    abort_if_not_wtdt(wt)
    wt[, tmp_index := 1:.N, by = panelist_id]
    wt[as.numeric(data.table::shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp) > cutoff, session := 1:.N, by = "panelist_id"]
    wt[, session := ifelse(tmp_index == 1, 1, session)]
    data.table::setnafill(wt, type = "locf", cols = "session")
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
#' @param add_grpvars vector. If method set to `"aggregate"`, determines
#' whether any additional variables are included in grouping of visits and
#' therefore kept. Defaults to `NULL`.
#' @return webtrack data.frame with the same columns as wt with updated duration
#' @examples
#' \dontrun{
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
#' # Aggregating duplicates and keeping "domain" variable despite grouping
#' wt <- extract_domain(wt)
#' wt_dedup <- deduplicate(wt, method = "aggregate", add_grpvars = "domain")
#' }
#' @export
#'
deduplicate <- function(wt, method = "aggregate", within = 1, duration_var = "duration",
                        keep_nvisits = FALSE, same_day = TRUE, add_grpvars = NULL) {
    abort_if_not_wtdt(wt)

    if (method == "aggregate") {
        vars_exist(wt, vars = duration_var)
        setnames(wt, duration_var, "duration")
        wt[, visit := cumsum(url != data.table::shift(url, n = 1, type = "lag", fill = 0)), by = "panelist_id"]

        if (same_day == TRUE) {
            wt[, day := as.Date(timestamp)]
            grp_vars <- c("panelist_id", "visit", "url", "day")
            if (!is.null(add_grpvars)) grp_vars <- c(grp_vars, add_grpvars)

            wt <- wt[, list(
                visits = .N,
                duration = sum(as.numeric(duration), na.rm = TRUE),
                timestamp = min(timestamp)
            ), by = grp_vars]
            wt[, day := NULL]
        } else {
            grp_vars <- c("panelist_id", "visit", "url")
            if (!is.null(add_grpvars)) grp_vars <- c(grp_vars, add_grpvars)
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
        data.table::setnames(wt, "duration", duration_var)
    } else if (method %in% c("drop", "flag")) {
        stopifnot("'within' must be specified if 'method' set to 'flag' or 'drop" = !is.null(within))

        wt[, tmp_timestamp_prev := data.table::shift(timestamp, n = 1, type = "lag", fill = NA), by = "panelist_id"]
        wt[, tmp_url_prev := data.table::shift(url, n = 1, type = "lag", fill = NA), by = "panelist_id"]
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
    # class(wt) <- c("wt_dt", class(wt))
    wt[]
}

#' Extract the host from URL
#' @description
#' `extract_host()` adds the host of a URL as a new column.
#' The host is defined as the part following the scheme (e.g., "https://") and
#' preceding the subdirectory (anything following the next "/"). Note that
#' for URL entries like `chrome-extension://soomething` or `http://192.168.0.1/something`,
#' result will be set to `NA`.
#' @param wt webtrack data object.
#' @param varname character. Name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @return webtrack data.frame with the same columns as wt
#' and a new column called `'host'` (or, if varname not equal to `'url'`, `'<varname>_host'`)
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract host and drop rows without host
#' wt <- extract_host(wt)
#' # Extract host and keep rows without host
#' wt <- extract_host(wt)
#' }
#' @export
extract_host <- function(wt, varname = "url") {
    abort_if_not_wtdt(wt)
    vars_exist(wt, varname)
    host <- adaR::ada_get_hostname(wt[[varname]])
    if (varname == "url") {
        wt[["host"]] <- host
    } else {
        wt[[paste0(varname, "_host")]] <- host
    }
    wt
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
#' For details, see [here](https://github.com/google/guava/wiki/InternetDomainNameExplained)
#' @param wt webtrack data object.
#' @param varname character. Name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @description Extracts the domain from urls.
#' @return webtrack data.frame with the same columns as wt
#' and a new column called `'domain'`
#' (or, if varname not equal to `'url'`, `'<varname>_domain'`)
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract domain and drop rows without domain
#' wt <- extract_domain(wt)
#' # Extract domain and keep rows without domain
#' wt <- extract_domain(wt)
#' }
#' @export
extract_domain <- function(wt, varname = "url") {
    abort_if_not_wtdt(wt)
    vars_exist(wt, varname)
    protocol <- adaR::ada_get_protocol(wt[[varname]])
    wt[[varname]][is.na(protocol)] <- paste0("https://", wt[[varname]][is.na(protocol)])
    domain <- adaR::ada_get_domain(wt[[varname]])
    if (varname == "url") {
        wt[["domain"]] <- domain
    } else {
        wt[[paste0(varname, "_domain")]] <- domain
    }
    wt
}

#' Extract the path from URL
#' @description
#' `extract_path()` adds the path of a URL as a new column.
#' The path is defined as the part following the host but not including a
#' query (anything after a "?") or a fragment (anything after a "#").
#' @param wt webtrack data object
#' @param varname character. name of the column from which to extract the host. Defaults to `"url"`.
#' @param decode logical. Whether to decode the path (see [utils::URLdecode()]), default to TRUE
#' @return webtrack data.frame with the same columns as wt
#' and a new column called `'path'` (or, if varname not equal to `'url'`, `'<varname>_path'`)
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract path
#' wt <- extract_path(wt)
#' }
#' @export
extract_path <- function(wt, varname = "url", decode = TRUE) {
    abort_if_not_wtdt(wt)
    vars_exist(wt, varname)
    path <- adaR::ada_get_pathname(wt[[varname]], decode = decode)
    if (varname == "url") {
        wt[["path"]] <- path
    } else {
        wt[[paste0(varname, "_path")]] <- path
    }
    wt
}

#' Drop the query and fragment from URL
#' @description
#' `drop_query()` adds the URL without query and fragment as a new column.
#' The query is defined as the part following a "?" after the path.
#' The fragement is anything following a "#" after the query.
#' @param wt webtrack data object.
#' @param varname character. name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @return webtrack data.frame with the same columns as wt
#' and a new column called `'<varname>_noquery'`
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Extract URL without query/fragment
#' wt <- drop_query(wt)
#' }
#' @export
drop_query <- function(wt, varname = "url") {
    abort_if_not_wtdt(wt)
    vars_exist(wt, varname)
    wt[[paste0(varname, "_noquery")]] <- adaR::ada_clear_hash(adaR::ada_clear_search(wt[[varname]]))
    return(wt)
}

#' Parse parts of path for text analysis
#' @description
#' `parse_path()` parses parts of a path, i.e., anything separated by
#' "/", "-", "_" or ".", and adds them as a new variable. Parts that do not
#' consist of letters only, or of a real word, can be filtered via the argument `keep`.
#' @param wt webtrack data object
#' @param varname character. name of the column from which to extract the host.
#' Defaults to `"url"`.
#' @param keep character. Defines which types of path components to keep.
#' If set to `"all"`, anything is kept. If `"letters_only"`, only parts
#' containing letters are kept. If `"words_only"`, only parts constituting
#' English words (as defined by the Word Game Dictionary,
#' cf. https://cran.r-project.org/web/packages/words/index.html) are kept.
#' Support for more languages will be added in future.
#' @param decode logical. Whether to decode the path (see [utils::URLdecode()]), default to TRUE
#' @return webtrack data.frame with the same columns as wt
#' and a new column called `'path_split'`  (or, if varname not equal to `'url'`, `'<varname>_path_split'`)
#' containing parts as a comma-separated string.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- parse_path(wt)
#' }
#' @export
parse_path <- function(wt, varname = "url", keep = "letters_only", decode = TRUE) {
    abort_if_not_wtdt(wt)
    vars_exist(wt, varname)
    keep <- match.arg(keep, c("letters_only", "words_only"))

    path_delims <- "/|-|_|\\."
    if (!"path" %in% names(wt)) {
        tmp <- extract_path(wt, varname, decode = decode)
        paths <- tmp[[grep("path", names(tmp))]]
    } else {
        paths <- wt[["path"]]
    }
    paths <- sub("^/", "", paths)
    path_split <- strsplit(paths, path_delims)
    if (keep == "letters_only") {
        path_split <- lapply(path_split, function(x) x[grepl("^[A-Za-z]+$", x)])
    } else if (keep == "words_only") {
        words <- words_en$word
        path_split <- lapply(path_split, function(x) x[x %fin% words])
    }
    parsed <- vapply(path_split, function(x) paste0(x, collapse = ","), character(1))
    if (varname == "url") {
        wt[["path_split"]] <- parsed
    } else {
        wt[[paste0(varname, "_path_split")]] <- parsed
    }
    wt
}

#' Add the next visit as a new column
#' @description
#' `add_next_visit()` adds the subsequent visit, as determined by order of
#' timestamps as a new column. The next visit can be added as either the full URL,
#' the extracted host or the extracted domain, depending on `level`.
#' @param wt webtrack data object.
#' @param level character. Either `"url"`, `"host"` or `"domain"`. Defaults to `"url"`.
#' @return webtrack data.frame with the same columns as wt and
#' a new column called `url_next`,`host_next` or `domain_next`.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Adding next full URL as new column
#' wt <- add_next_visit(wt, level = "url")
#' # Adding next host as new column
#' wt <- add_next_visit(wt, level = "host")
#' # Adding next domain as new column
#' wt <- add_next_visit(wt, level = "domain")
#' }
#' @export
add_next_visit <- function(wt, level = "url") {
    abort_if_not_wtdt(wt)
    level <- match.arg(level, c("url", "host", "domain"))

    setorder(wt, panelist_id, timestamp)

    if (level == "url") {
        wt[, url_next := data.table::shift(url, n = 1, type = "lead", fill = NA), by = "panelist_id"]
    } else if (level == "host") {
        if (!"host" %in% names(wt)) {
            wt <- extract_host(wt, varname = "url")
            wt[, host_next := data.table::shift(host, n = 1, type = "lead", fill = NA), by = "panelist_id"]
            wt[, host := NULL]
        } else {
            wt[, host_next := data.table::shift(host, n = 1, type = "lead", fill = NA), by = "panelist_id"]
        }
    } else if (level == "domain") {
        if (!"domain" %in% names(wt)) {
            wt <- extract_domain(wt, varname = "url")
            wt[, domain_next := data.table::shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
            wt[, domain := NULL]
        } else {
            wt[, domain_next := data.table::shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
        }
    }
    wt[]
}

#' Add the previous visit as a new column
#' @description
#' `add_previous_visit()` adds the previous visit, as determined by order of
#' timestamps as a new column The previous visit can be added as either the full URL,
#' the extracted host or the extracted domain, depending on `level`.
#' @param wt webtrack data object.
#' @param level character. Either `"url"`, `"host"` or `"domain"`. Defaults to `"url"`.
#' @return webtrack data.frame with the same columns as wt and
#' a new column called `url_previous`,`host_previous` or `domain_previous.`.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # Adding previous full URL as new column
#' wt <- add_previous_visit(wt, level = "url")
#' # Adding previous host as new column
#' wt <- add_previous_visit(wt, level = "host")
#' # Adding previous domain as new column
#' wt <- add_previous_visit(wt, level = "domain")
#' }
#' @export
add_previous_visit <- function(wt, level = "url") {
    abort_if_not_wtdt(wt)
    level <- match.arg(level, c("url", "host", "domain"))
    setorder(wt, panelist_id, timestamp)

    if (level == "url") {
        wt[, url_previous := data.table::shift(url, n = 1, type = "lag", fill = NA), by = "panelist_id"]
    } else if (level == "host") {
        if (!"host" %in% names(wt)) {
            wt <- extract_host(wt, varname = "url")
            wt[, host_previous := data.table::shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
            wt[, host := NULL]
        } else {
            wt[, host_previous := data.table::shift(host, n = 1, type = "lag", fill = NA), by = "panelist_id"]
        }
    } else if (level == "domain") {
        if (!"domain" %in% names(wt)) {
            wt <- extract_domain(wt, varname = "url")
            wt[, domain_previous := data.table::shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
            wt[, domain := NULL]
        } else {
            wt[, domain_previous := data.table::shift(domain, n = 1, type = "lag", fill = NA), by = "panelist_id"]
        }
    }
    wt[]
}

#' Download and add the "title" of a URL
#' @description
#' Gets the title of a URL by accessing the web address online
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
#' @return webtrack data.frame with the same columns as wt and a new column
#' called `"title"`, which will be `NA` if the title cannot be retrieved.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)[1:2]
#' # Get titles with `lang` set to default English
#' wt_titles <- add_title(wt)
#' # Get titles with `lang` set to German
#' wt_titles <- add_title(wt, lang = "de")
#' }
#' @export
add_title <- function(wt, lang = "en-US,en-GB,en") {
    abort_if_not_wtdt(wt)

    urls <- data.frame(url = unique(wt$url))

    urls$title <- mapply(function(x) {
        return(
            tryCatch(
                {
                    # Getting content
                    resp <- httr::GET(x, httr::add_headers(.headers = c(
                        "user_agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36 Edge/12.246",
                        "Accept-language" = lang
                    )))
                    content <- content(resp, as = "text", encoding = "UTF-8")

                    # Extracting title using base R functions
                    title_pattern <- "<title>(.*?)</title>"
                    title_matches <- regmatches(content, regexpr(title_pattern, content, ignore.case = TRUE))
                    if (length(title_matches) > 0 && nchar(title_matches[[1]]) > 0) {
                        sub(title_pattern, "\\1", title_matches[[1]], ignore.case = TRUE)
                    } else {
                        NA
                    }
                },
                error = function(e) NA
            )
        )
    }, urls$url)

    on.exit(closeAllConnections())

    wt <- merge(wt, urls, by = "url", all.x = TRUE)

    return(wt)
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
#' @return webtrack data.frame with the same columns as wt and a new column called `referral`,
#' which takes on NA if no referral has been identified, or the name specified
#' platform_domains if a referral from that platform has been identified
#' @references
#' Schmidt, Felix, Frank Mangold, Sebastian Stier and Roberto Ulloa. "Facebook as an Avenue to News: A Comparison and Validation of Approaches to Identify Facebook Referrals". Working paper.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
#' wt <- add_referral(wt,
#'     platform_domains = c("facebook.com", "twitter.com"),
#'     patterns = c("fbclid=", "utm_source=twitter")
#' )
#' }
#' @export
add_referral <- function(wt, platform_domains, patterns) {
    abort_if_not_wtdt(wt)
    stopifnot("Number of platform_domains must be identical to number of patterns" = length(platform_domains) == length(patterns))
    wt <- add_previous_visit(wt, level = "domain")
    wt$referral <- NA

    conditions_matrix <- sapply(seq_along(patterns), function(i) {
        grepl(patterns[i], wt$url) & wt$domain_previous == platform_domains[i]
    })

    wt$referral <- apply(conditions_matrix, 1, function(row) {
        idx <- which(row)[1]
        if (!is.na(idx)) {
            return(platform_domains[idx])
        } else {
            return(NA)
        }
    })

    wt$domain_previous <- NULL

    return(wt)
}

#' Create an urldummy variable
#' @param wt webtrack data object
#' @param dummy a vector of urls that should be dummy coded
#' @param name name of dummy variable to create.
#' @importFrom fastmatch %fin%
#' @return webtrack object with the same columns and a new column called "name" including the dummy variable
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
#' code_urls <- "https://dkr1.ssisurveys.com/tzktsxomta"
#' create_urldummy(wt, dummy = code_urls, name = "test_dummy")
#' }
#' @export
create_urldummy <- function(wt, dummy, name) {
    abort_if_not_wtdt(wt)
    vars_exist(wt, "url")
    wt[[name]] <- wt$url %fin% dummy
    wt
}


#' Add panelist features to tracking data
#' @description
#' Adds information about panelists (e.g., from a survey)
#' to the tracking data.
#' @param wt webtrack data object.
#' @param data a data frame containing panelist data
#'  which contains columns about panelists
#' @param cols character vector of columns to add. If `NULL`, all columns are added.
#' Defaults to `NULL`.
#' @param join_on which columns to join on. Defaults to `"panelist_id"`.
#' @return webtrack object with the same columns and the columns from `data`
#' specified in `cols`.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' data("testdt_survey_w")
#' wt <- as.wt_dt(testdt_tracking)
#' # add survey test data
#' add_panelist_data(wt, testdt_survey_w)
#' }
#' @export
add_panelist_data <- function(wt, data, cols = NULL, join_on = "panelist_id") {
    abort_if_not_wtdt(wt)
    if (!is.null(cols)) {
        if (!all(cols %in% names(data))) {
            stop("couldn't locate all columns in data")
        }
        data <- data[, c(join_on, cols)]
    }

    merged_data <- merge(wt, data, by = join_on, all.x = TRUE)

    return(merged_data)
}

# helpers
lead <- function(x, n = 1, default = NA) {
    if (length(x) <= n) {
        return(rep(default, length(x)))
    }
    c(tail(x, -n), rep(default, n))
}

lag <- function(x, n = 1, default = NA) {
    if (length(x) <= n) {
        return(rep(default, length(x)))
    }
    c(rep(default, n), head(x, -n))
}

fill_na_locf <- function(x) {
    na_loc <- which(is.na(x))
    for (i in na_loc) {
        x[i] <- x[i - 1]
    }
    return(x)
}
