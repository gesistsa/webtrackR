#' Summarize number of visits by person
#' @description Summarize number of visits by person within a time frame, and optionally by class of visit
#' @details sum_visits allows you to summarize the number of visits by panelist_id for different time periods (for example, by day).
#' It further allows to break down the number by any set of "classes" of visits, e.g. the type of the visit's domain.
#' @param wt webtrack data object.
#' @param timeframe character. indicates for what time frame to aggregate visits. Possible values are "date", "week", "month", "year", "wave" or "all".
#' If set to "wave", webtrack data object must contain a column call "wave". Defaults to "all".
#' @param visit_class character vector. Column(s) that contains a classification of visits.
#' Visits will be grouped by values in this column before being summarized. Defaults to NULL.
#' @importFrom data.table is.data.table shift .N
#' @return a data.table with columns "panelist_id", column indicating the time unit unless "all" was specified,
#' name indicating the class variable if specified, and "n_visits" indicating the number of visits
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # example of visit classification
#' wt <- extract_domain(wt)
#' wt[,google:=ifelse(domain == "google.com", 1, 0)]
#' wt[,search:=ifelse(grepl("search", url), 1, 0)]
#' summary <- sum_visits(wt, timeframe = "month", visit_class = c("google", "search"))
#' @export
sum_visits <- function(wt, timeframe = "all", visit_class = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  timeframe <- match.arg(timeframe, c("all", "date", "week", "month", "year", "wave"))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  if (timeframe == "all") {
    timeframe_var <- NULL
  } else if (timeframe == "date") {
    wt[, `:=`(date = format(timestamp, format = "%F"))]
    timeframe_var <- "date"
  } else if (timeframe == "week") {
    wt[, `:=`(week = format(timestamp, format = "%Y week %W"))]
    timeframe_var <- "week"
  } else if (timeframe == "month") {
    wt[, `:=`(month = format(timestamp, format = "%Y month %m"))]
    timeframe_var <- "month"
  } else if (timeframe == "year") {
    wt[, `:=`(year = format(timestamp, format = "%Y"))]
    timeframe_var <- "year"
  } else if (timeframe == "wave") {
    vars_wt <- names(wt)
    wave <- pmatch("wave", vars_wt)
    if (is.na(wave)) {
      stop("couldn't find the column 'wave' in the webtrack data", call. = FALSE)
    } else {
      timeframe_var <- "wave"
    }
  }
  grp_vars <- c("panelist_id", timeframe_var, visit_class)
  summary <- wt[, list("n_visits" = .N), by = grp_vars]
  summary[]
}

#' Summarize visit duration by person
#' @description Summarize duration of visits by person within a time frame, and optionally by class of visit
#' @details sum_durations allows you to sum the duration of visits by panelist_id for different time periods (for example, by day).
#' It further allows to break down the number by any set of "classes" of visits, e.g. the type of the visit's domain.
#' @param wt webtrack data object.
#' @param var_duration character. Name of the duration variable if already present. Defaults to NULL,
#' in which case duration will be approximated with add_duration(wt, cutoff = 300, replace_by = "na", replace_val = NULL)
#' @param timeframe character. Indicates for what time frame to aggregate visits. Possible values are "date", "week", "month", "year", "wave" or "all".
#' If set to "wave", webtrack data object must contain a column call "wave". Defaults to "all".
#' @param visit_class character vector. Column(s) that contains a classification of visits.
#' Visits will be grouped by values in this column before being summarized. Defaults to NULL.
#' @importFrom data.table is.data.table shift .N
#' @return a data.table with columns "panelist_id", column indicating the time unit unless "all" was specified,
#' name indicating the class variable if specified, and "duration_visits" indicating the duration of visits
#' (in seconds, or whatever the unit of the variable specified by "var_duration" parameter)
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # example of visit classification
#' wt <- extract_domain(wt)
#' wt[,google:=ifelse(domain == "google.com", 1, 0)]
#' wt[,search:=ifelse(grepl("search", url), 1, 0)]
#' summary <- sum_durations(wt, var_duration = NULL, timeframe = "month", visit_class = c("google", "search"))
#' @export
sum_durations <- function(wt, var_duration = NULL, timeframe = "all", visit_class = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  timeframe <- match.arg(timeframe, c("all", "date", "week", "month", "year", "wave"))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  if (is.null(var_duration)) {
    wt <- add_duration(wt)
  } else {
    vars_exist(wt, vars = c(var_duration))
    colnames(wt)[colnames(wt) == var_duration] <- "duration"
  }
  if (timeframe == "all") {
    timeframe_var <- NULL
  } else if (timeframe == "date") {
    wt[, `:=`(date = format(timestamp, format = "%F"))]
    timeframe_var <- "date"
  } else if (timeframe == "week") {
    wt[, `:=`(week = format(timestamp, format = "%Y week %W"))]
    timeframe_var <- "week"
  } else if (timeframe == "month") {
    wt[, `:=`(month = format(timestamp, format = "%Y month %m"))]
    timeframe_var <- "month"
  } else if (timeframe == "year") {
    wt[, `:=`(year = format(timestamp, format = "%Y"))]
    timeframe_var <- "year"
  } else if (timeframe == "wave") {
    vars_wt <- names(wt)
    wave <- pmatch("wave", vars_wt)
    if (is.na(wave)) {
      stop("couldn't find the column 'wave' in the webtrack data", call. = FALSE)
    } else {
      timeframe_var <- "wave"
    }
  }
  grp_vars <- c("panelist_id", timeframe_var, visit_class)
  summary <- wt[, list("duration_visits" = sum(duration, na.rm = T)), by = grp_vars]
  summary[]
}

#' Summarize activity per person
#' @description Count number of active time periods (days, weeks, months, years, or waves) per person
#' @details sum_activity allows you to count the number of active days, weeks, months, years or waves by panelist_id.
#' A period counts as "active" if the person provided at least one visit for that period.
#' @param wt webtrack data object.
#' @param timeframe character. Indicates for what time frame to aggregate visits. Possible values are "date", "week", "month", "year", or "wave".
#' If set to "wave", webtrack data object must contain a column call "wave". Defaults to "date".
#' @importFrom data.table is.data.table shift .N
#' @return a data.table with columns "panelist_id" and column indicating the number of active time units.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt_sum <- sum_activity(wt, timeframe = "date")
#' @export
sum_activity <- function(wt, timeframe = "date") {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  timeframe <- match.arg(timeframe, c("date", "week", "month", "year", "wave"))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  if (timeframe == "date") {
    wt[, `:=`(date = format(timestamp, format = "%F"))]
    timeframe_var <- "active_dates"
  } else if (timeframe == "week") {
    wt[, `:=`(week = format(timestamp, format = "%Y week %W"))]
    timeframe_var <- "active_weeks"
  } else if (timeframe == "month") {
    wt[, `:=`(month = format(timestamp, format = "%Y month %m"))]
    timeframe_var <- "active_months"
  } else if (timeframe == "year") {
    wt[, `:=`(year = format(timestamp, format = "%Y"))]
    timeframe_var <- "active_years"
  } else if (timeframe == "wave") {
    vars_wt <- names(wt)
    wave <- pmatch("wave", vars_wt)
    if (is.na(wave)) {
      stop("couldn't find the column 'wave' in the webtrack data", call. = FALSE)
    } else {
      timeframe_var <- "active_waves"
    }
  }
  summary <- wt[, list(temp = length(unique(get(timeframe)))), by = "panelist_id"]
  data.table::setnames(summary, "temp", timeframe_var)
  summary[]
}

#' Aggregate duration of consecutive visits to a URL
#' @description Aggregate the duration of consecutive visits by a person to the same URL
#' @param wt webtrack data object
#' @param keep logical. If number of aggregated visits should be kept as variable. Defaults to FALSE
#' @param same_day logical. Whether to count visits as consecutive only when on the same day. Defaults to TRUE
#' @param duration_var logical. Name of duration variable. Defaults to "duration".
#' @importFrom data.table is.data.table shift .N
#' @return webtrack data.table with the same columns as wt with updated duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt, replace_by = "cutoff")
#' agg_duration(wt)
#' @export
agg_duration <- function(wt, keep = FALSE, same_day = TRUE, duration_var = "duration"){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("url","panelist_id","timestamp", duration_var))
  data.table::setnames(wt, duration_var, "duration")
  wt[, visit := cumsum(url != shift(url, n = 1, type = "lag", fill = 0)), by = "panelist_id"]
  grp_vars <- c("panelist_id", "visit", "url")
  if (same_day == TRUE) {
    wt[, day := as.Date(timestamp)]
    grp_vars <- c(grp_vars, "day")
  }
  wt <- wt[, .(visits = .N,
                    duration = sum(as.numeric(duration), na.rm = TRUE),
                    timestamp = min(timestamp)), by = grp_vars]
  wt[,visit:=NULL]
  if (same_day == TRUE) {wt[,day:=NULL]}
  if (keep == FALSE) {wt[, visits := NULL]}
  data.table::setnames(wt, "duration", duration_var)
  wt[]
}

