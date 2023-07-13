#' Add time spent on a visit in seconds
#' @description Approximate the time spent on a visit based on the sequence of timestamps
#' @param wt webtrack data object
#' @param cutoff numeric. If duration is greater than this value, it is reset to na, the cutoff, or a user-defined value. Defaults to 5 minutes (= 300 seconds).
#' @param replace_by numeric. This determines whether differences greater than
#' the cutoff, as well as the last visit for an individual, are set to NA
#' (default), or a numeric value
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table (ordered by panelist_id and timestamp) with the same columns as wt and a new column called duration
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt)
#' # Defining cutoff at 10 minutes and setting critical visits to the cutoff:
#' wt <- add_duration(wt, cutoff = 600, replace_by = 600)
#' # Defining cutoff at 10 minutes and setting critical visits to 5 minutes:
#' wt <- add_duration(wt, cutoff = 600, replace_by = 300)
#' }
#' @export
add_duration <- function(wt, cutoff = 300, replace_by = NA) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  stopifnot("replace_by must be NA or greater zero" = (is.na(replace_by) | replace_by > 0))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  data.table::setorder(wt, panelist_id, timestamp)
  wt[, duration := as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA) - timestamp), by = "panelist_id"]
  wt[is.na(duration), duration := replace_by]
  wt[duration > cutoff, duration := replace_by]
  wt[]
}

#' Create a session variable
#' @description Define sessions of browsing depending on aq time cutoff for inactivity
#' @param wt webtrack data object
#' @param cutoff numeric. If the consecutive visit happens later than this value (in seconds), a new browsing session is defined
#' @importFrom data.table is.data.table shift setorder setnafill
#' @return webtrack data.table (ordered by panelist_id and timestamp) with the same columns as wt and a new column called duration
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_session(wt, cutoff = 1800)
#' }
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
#' @description Drop consecutive visits that are to the same URL within a user-defined timeframe
#' @param wt webtrack data object
#' @param within numeric. If the consecutive visit happens within than this time difference (in seconds), it is flagged as a duplicate. Defaults to 1.
#' @param drop numeric. If duplicate visits should be dropped (TRUE), or just flagged with a new variable (FALSE). Defaults to FALSE.
#' @importFrom data.table is.data.table shift setorder
#' @return webtrack data.table with the same columns as wt and, if drop set to FALSE, a new column called "duplicate"
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt1 <- as.wt_dt(testdt_tracking)[1:1000] # revisit with new example data
#' wt2 <- as.wt_dt(testdt_tracking)[1:1000]
#' wt <- data.table::rbindlist(list(wt1, wt2))
#' wt <- as.wt_dt(wt)
#' wt <- deduplicate(wt)
#' }
#' @export
deduplicate <- function(wt, within = 1, drop = FALSE) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  stopifnot("'within' must be numeric" = is.numeric(within))
  vars_exist(wt, vars = c("panelist_id", "timestamp"))
  setorder(wt, panelist_id, timestamp)
  wt[, tmp_timestamp_next := shift(timestamp, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  wt[, tmp_url_next := shift(url, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  wt[, duplicate := ifelse(((tmp_timestamp_next - timestamp <= within) & (url == tmp_url_next)), TRUE, FALSE), by = "panelist_id"]
  if (drop == TRUE) {
    wt <- wt[duplicate == FALSE]
    wt[, duplicate := NULL]
  }
  wt[, tmp_url_next := NULL]
  wt[, tmp_timestamp_next := NULL]
  wt[]
}

#' Extract host from url
#' @description Extracts the host from urls. The host is defined as the part between the scheme (e.g., "https://") and the subdirectory.
#' @param wt webtrack data object
#' @param varname character. name of the URL variable from which to extract the host. Defaults to "url".
#' @param drop_na boolean. Whether rows for which no host can be extracted should be dropped from the data. Defaults to TRUE.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called "[url]_domain"
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_host(wt)
#' }
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
    warning(paste0("Host could not be extracted for ", n_na, " rows, which were dropped from the data. Set drop_na = FALSE to keep these rows."))
  } else {
    warning(paste0("Host could not be extracted for ", n_na, " rows. Set drop_na = TRUE to drop these rows."))
  }
  wt[]
}

#' Extract domain from url
#' @description Extracts the domain from urls. We define the domain (e.g., "google.com") as the sum of the suffix (e.g., ".com") and the part before that and the preceding dot (e.g., "google" in "https://mail.google.com).
#' @param wt webtrack data object
#' @param varname name of the URL variable from which to extract the domain Defaults to "url".
#' @param drop_na boolean. Whether rows for which no domain can be extracted should be dropped from the data. Defaults to TRUE.
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called "<varname>_domain"
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
#' }
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
    warning(paste0("Domain could not be extracted for ", n_na, " rows, which were dropped from the data. Set drop_na = FALSE to keep these rows."))
  } else {
    warning(paste0("Domain could not be extracted for ", n_na, " rows. Set drop_na = TRUE to drop these rows."))
  }
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
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- drop_query(wt)
#' }
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
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_next_visit(wt, level = "url")
#' wt <- add_next_visit(wt, level = "host")
#' wt <- add_next_visit(wt, level = "domain")
#' }
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
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_previous_visit(wt, level = "url")
#' wt <- add_previous_visit(wt, level = "host")
#' wt <- add_previous_visit(wt, level = "domain")
#' }
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
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called "title",
#' which will be NA if the title cannot be retrieved.
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_title(wt)
#' }
#' @export
add_title <- function(wt) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt, vars = c("panelist_id", "url"))
  urls <- data.table(url = unique(wt$url))
  urls[, title := mapply(function(x) {
    return(
      tryCatch(
        rvest::html_text(rvest::html_node(rvest::read_html(x), "head title")),
        error = function(e) NA
      )
    )
  }, url)]
  closeAllConnections()
  wt <- wt[urls, on = "url"]
  wt[]
}

#' Create an urldummy variable from a data.table object
#' @param wt webtrack data object
#' @param dummy a vector of urls that should be dummy coded
#' @param name name of dummy variable to create.
#' @importFrom data.table setnames setattr
#' @return webtrack object with the same columns and a new column called "name" including the dummy variable
#' @examples
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_domain(wt)
#' code_urls <- c("Ccj4QELzbJe6.com/FrKrkvugBVJWwfSobV")
#' create_urldummy(wt, dummy = code_urls, name = "test_dummy")
#' }
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
#' \dontrun{
#' data("testdt_tracking")
#' data("testdt_survey_w")
#' wt <- as.wt_dt(testdt_tracking)
#' add_panelist_data(wt, testdt_survey_w)
#' }
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
