#' Summarize number of visits by person
#' @description
#' `sum_visits()` summarizes the number of visits by person within a `timeframe`,
#' and optionally by `visit_class` of visit.
#' @param wt webtrack data object.
#' @param timeframe character. Indicates for what time frame to aggregate visits.
#' Possible values are `"date"`, `"week"`, `"month"`, `"year"`, `"wave"` or `NULL`.
#' If set to `"wave"`, `wt` must contain a column call `wave`. Defaults to `NULL`,
#' in which case the output contains number of visits for the entire time.
#' @param visit_class character. Column that contains a classification of visits.
#' For each value in this column, the output will have a column indicating the
#' number of visits belonging to that value. Defaults to `NULL`.
#' @importFrom data.table is.data.table shift .N dcast setnames
#' @return a data.table with columns `panelist_id`, column indicating the time unit
#' (unless `timeframe` set to `NULL`), `n_visits` indicating the number of visits,
#' and a column for each value of `visit_class`, if specified.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # summarize for whole period
#' wt_summ <- sum_visits(wt)
#' # summarize by week
#' wt_summ <- sum_visits(wt, timeframe = "week")
#' # create a class variable to summarize by class
#' wt <- suppressWarnings(extract_domain(wt, drop_na = TRUE))
#' wt[, google := ifelse(domain == "google.com", 1, 0)]
#' wt_summ <- sum_visits(wt, timeframe = "week", visit_class = "google")
#' @export
sum_visits <- function(wt, timeframe = NULL, visit_class = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  match.arg(timeframe, c(NULL, "date", "week", "month", "year", "wave"))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  if (!is.null(timeframe)) {
    if (timeframe == "date") {
      wt[, `:=`(date = format(timestamp, format = "%F"))]
    } else if (timeframe == "week") {
      wt[, `:=`(week = format(timestamp, format = "%Y week %W"))]
    } else if (timeframe == "month") {
      wt[, `:=`(month = format(timestamp, format = "%Y month %m"))]
    } else if (timeframe == "year") {
      wt[, `:=`(year = format(timestamp, format = "%Y"))]
    } else if (timeframe == "wave") {
      stopifnot("couldn't find the column 'wave' in the webtrack data" = "wave" %in% names(wt))
    }
  }
  grp_vars <- c("panelist_id", timeframe)
  summary <- wt[, list("n_visits" = .N), by = grp_vars]
  if (!is.null(visit_class)) {
    wt <- wt[, tmp_class := paste0("n_visits_", visit_class, "_", get(visit_class))]
    grp_vars <- c("panelist_id", timeframe, "tmp_class")
    tmp_summary <- wt[, list("tmp_visits" = .N), by = grp_vars]
    if (is.null(timeframe)) {
      tmp_summary <- dcast(tmp_summary, panelist_id ~ tmp_class, value.var = c("tmp_visits"), fill = 0)
    } else {
      setnames(tmp_summary, timeframe, "tmp_timeframe")
      tmp_summary <- dcast(tmp_summary, panelist_id + tmp_timeframe ~ tmp_class, value.var = c("tmp_visits"), fill = 0)
      setnames(tmp_summary, "tmp_timeframe", timeframe)
    }
    wt[, tmp_class := NULL]
    summary <- summary[tmp_summary, on = c("panelist_id", timeframe)]
  }
  if (!is.null(timeframe)) {
    wt[[timeframe]] <- NULL
  }
  summary[]
}

#' Summarize visit duration by person
#' @description
#' `sum_durations()` summarizes the duration of visits by person within a `timeframe`,
#' and optionally by `visit_class` of visit. Note:
#' - If for a time frame all rows are NA on the duration column, the summarized duration for that time frame will be NA.
#' - If only some of the rows of a time frame are NA on the duration column, the function will ignore those NA rows.
#' - If there were no visits to a class (i.e., a value of the 'visit_class' column) for a time frame, the summarized duration for that time frame will be zero; if there were visits, but NA on duration, the summarized duration will be NA.
#' @param wt webtrack data object.
#' @param var_duration character. Name of the duration variable if already present.
#' Defaults to `NULL`, in which case duration will be approximated with
#' `add_duration(wt, cutoff = 300, replace_by = "na", replace_val = NULL)`
#' @param timeframe character. Indicates for what time frame to aggregate visit durations.
#' Possible values are `"date"`, `"week"`, `"month"`, `"year"`, `"wave"` or `NULL`.
#' If set to `"wave"`, `wt` must contain a column call `wave`. Defaults to `NULL`,
#' in which case the output contains duration of visits for the entire time.
#' @param visit_class character. Column that contains a classification of visits.
#' For each value in this column, the output will have a column indicating the
#' number of visits belonging to that value. Defaults to `NULL`.
#' @importFrom data.table is.data.table shift .N dcast setnames
#' @return a data.table with columns `panelist_id`, column indicating the time unit
#' (unless `timeframe` set to `NULL`), `duration_visits` indicating the duration of visits
#' (in seconds, or whatever the unit of the variable specified by `var_duration` parameter),
#' and a column for each value of `visit_class`, if specified.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # summarize for whole period
#' wt_summ <- sum_durations(wt)
#' # summarize by week
#' wt_summ <- sum_durations(wt, timeframe = "week")
#' # create a class variable to summarize by class
#' wt <- suppressWarnings(extract_domain(wt, drop_na = TRUE))
#' wt[, google := ifelse(domain == "google.com", 1, 0)]
#' wt_summ <- sum_durations(wt, timeframe = "week", visit_class = "google")
#' @export
sum_durations <- function(wt, var_duration = NULL, timeframe = NULL, visit_class = NULL) {
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  match.arg(timeframe, c("date", "week", "month", "year", "wave", NULL))
  vars_exist(wt, vars = c("url", "panelist_id", "timestamp"))
  if (is.null(var_duration)) {
    wt <- add_duration(wt)
  } else {
    vars_exist(wt, vars = c(var_duration))
    colnames(wt)[colnames(wt) == var_duration] <- "duration"
  }
  if (!is.null(timeframe)) {
    if (timeframe == "date") {
      wt[, `:=`(date = format(timestamp, format = "%F"))]
    } else if (timeframe == "week") {
      wt[, `:=`(week = format(timestamp, format = "%Y week %W"))]
    } else if (timeframe == "month") {
      wt[, `:=`(month = format(timestamp, format = "%Y month %m"))]
    } else if (timeframe == "year") {
      wt[, `:=`(year = format(timestamp, format = "%Y"))]
    } else if (timeframe == "wave") {
      stopifnot("couldn't find the column 'wave' in the webtrack data" = "wave" %in% names(wt))
    }
  }

  grp_vars <- c("panelist_id", timeframe)
  summary <- wt[, list("duration_visits" = if (all(is.na(duration))) NA_real_ else sum(duration, na.rm = TRUE)), by = grp_vars]
  if (!is.null(visit_class)) {
    wt <- wt[, tmp_class := paste0("duration_visits_", visit_class, "_", get(visit_class))]
    grp_vars <- c("panelist_id", timeframe, "tmp_class")
    tmp_summary <- wt[, list("tmp_duration" = if (all(is.na(duration))) NA_real_ else sum(duration, na.rm = TRUE)), by = grp_vars]
    if (is.null(timeframe)) {
      tmp_summary <- dcast(tmp_summary, panelist_id ~ tmp_class, value.var = c("tmp_duration"), fill = 0)
    } else {
      setnames(tmp_summary, timeframe, "tmp_timeframe")
      tmp_summary <- dcast(tmp_summary, panelist_id + tmp_timeframe ~ tmp_class, value.var = c("tmp_duration"), fill = 0)
      setnames(tmp_summary, "tmp_timeframe", timeframe)
    }
    wt[, tmp_class := NULL]
    summary <- summary[tmp_summary, on = c("panelist_id", timeframe)]
  }
  if (!is.null(timeframe)) {
    wt[[timeframe]] <- NULL
  }
  summary[]
}

#' Summarize activity per person
#' @description
#' `sum_activity()` counts the number of active time periods (i.e., days, weeks,
#' months, years, or waves) by `panelist_id`. A period counts as "active" if
#' the panelist provided at least one visit for that period.
#' @param wt webtrack data object.
#' @param timeframe character. Indicates for what time frame to aggregate visits.
#' Possible values are `"date"`, `"week"`, `"month"`, `"year"` or `"wave"`. If
#' set to `"wave"`, `wt` must contain a column call `wave`. Defaults to `"date"`.
#' @importFrom data.table is.data.table shift .N
#' @return a data.table with columns `panelist_id`, column indicating the
#' number of active time units.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # summarize activity by day
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
