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
#' \dontrun{
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
#' }
#' @export
sum_visits <- function(wt, timeframe = NULL, visit_class = NULL) {
    abort_if_not_wtdt(wt)
    match.arg(timeframe, c(NULL, "date", "week", "month", "year", "wave"))

    if (!is.null(timeframe)) {
        if (timeframe == "date") {
            wt$date <- format(wt$timestamp, format = "%F")
        } else if (timeframe == "week") {
            wt$week <- format(wt$timestamp, format = "%Y week %W")
        } else if (timeframe == "month") {
            wt$month <- format(wt$timestamp, format = "%Y month %m")
        } else if (timeframe == "year") {
            wt$year <- format(wt$timestamp, format = "%Y")
        } else if (timeframe == "wave") {
            stopifnot("couldn't find the column 'wave' in the webtrack data" = "wave" %in% names(wt))
        }
    }

    grp_vars <- c("panelist_id", timeframe)

    summary <- aggregate(x = list(n_visits = rep(1, nrow(wt))), by = wt[grp_vars], FUN = length)

    if (!is.null(visit_class)) {
        wt$tmp_class <- paste0("n_visits_", visit_class, "_", wt[[visit_class]])

        grp_vars <- c(grp_vars, "tmp_class")
        tmp_summary <- aggregate(x = list(tmp_visits = rep(1, nrow(wt))), by = wt[grp_vars], FUN = length)

        if (is.null(timeframe)) {
            tmp_summary <- reshape(tmp_summary, timevar = "tmp_class", idvar = "panelist_id", direction = "wide", sep = "_")
        } else {
            names(tmp_summary)[which(names(tmp_summary) == timeframe)] <- "tmp_timeframe"
            tmp_summary <- reshape(tmp_summary, timevar = "tmp_class", idvar = c("panelist_id", "tmp_timeframe"), direction = "wide", sep = "_")
            names(tmp_summary)[which(names(tmp_summary) == "tmp_timeframe")] <- timeframe
        }

        wt$tmp_class <- NULL

        summary <- merge(summary, tmp_summary, by = c("panelist_id", timeframe), all = TRUE)
    }

    if (!is.null(timeframe)) {
        wt[[timeframe]] <- NULL
    }

    return(summary)
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
#' \dontrun{
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
#' }
#' @export
sum_durations <- function(wt, var_duration = NULL, timeframe = NULL, visit_class = NULL) {
    abort_if_not_wtdt(wt)

    match.arg(timeframe, c("date", "week", "month", "year", "wave", NULL))

    if (is.null(var_duration)) {
        wt <- add_duration(wt)
    } else {
        vars_exist(wt, vars = c(var_duration))
        names(wt)[names(wt) == var_duration] <- "duration"
    }

    if (!is.null(timeframe)) {
        if (timeframe == "date") {
            wt$date <- format(wt$timestamp, format = "%F")
        } else if (timeframe == "week") {
            wt$week <- format(wt$timestamp, format = "%Y week %W")
        } else if (timeframe == "month") {
            wt$month <- format(wt$timestamp, format = "%Y month %m")
        } else if (timeframe == "year") {
            wt$year <- format(wt$timestamp, format = "%Y")
        } else if (timeframe == "wave") {
            stopifnot("couldn't find the column 'wave' in the webtrack data" = "wave" %in% names(wt))
        }
    }

    grp_vars <- c("panelist_id", timeframe)
    summary_fun <- function(duration) if (all(is.na(duration))) NA_real_ else sum(duration, na.rm = TRUE)
    summary <- aggregate(x = list(duration_visits = wt$duration), by = wt[grp_vars], FUN = summary_fun)

    if (!is.null(visit_class)) {
        wt$tmp_class <- paste0("duration_visits_", visit_class, "_", wt[[visit_class]])

        grp_vars <- c(grp_vars, "tmp_class")
        tmp_summary <- aggregate(x = list(tmp_duration = wt$duration), by = wt[grp_vars], FUN = summary_fun)

        if (is.null(timeframe)) {
            tmp_summary <- reshape(tmp_summary, timevar = "tmp_class", idvar = "panelist_id", direction = "wide", sep = "_")
        } else {
            names(tmp_summary)[which(names(tmp_summary) == timeframe)] <- "tmp_timeframe"
            tmp_summary <- reshape(tmp_summary, timevar = "tmp_class", idvar = c("panelist_id", "tmp_timeframe"), direction = "wide", sep = "_")
            names(tmp_summary)[which(names(tmp_summary) == "tmp_timeframe")] <- timeframe
        }

        wt$tmp_class <- NULL

        summary <- merge(summary, tmp_summary, by = c("panelist_id", timeframe), all = TRUE)
    }

    if (!is.null(timeframe)) {
        wt[[timeframe]] <- NULL
    }

    return(summary)
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
#' \dontrun{
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' # summarize activity by day
#' wt_sum <- sum_activity(wt, timeframe = "date")
#' }
#' @export
sum_activity <- function(wt, timeframe = "date") {
    abort_if_not_wtdt(wt)

    timeframe <- match.arg(timeframe, c("date", "week", "month", "year", "wave"))

    if (timeframe == "date") {
        wt$date <- format(wt$timestamp, format = "%F")
        timeframe_var <- "active_dates"
    } else if (timeframe == "week") {
        wt$week <- format(wt$timestamp, format = "%Y week %W")
        timeframe_var <- "active_weeks"
    } else if (timeframe == "month") {
        wt$month <- format(wt$timestamp, format = "%Y month %m")
        timeframe_var <- "active_months"
    } else if (timeframe == "year") {
        wt$year <- format(wt$timestamp, format = "%Y")
        timeframe_var <- "active_years"
    } else if (timeframe == "wave") {
        if ("wave" %in% names(wt)) {
            timeframe_var <- "active_waves"
        } else {
            stop("couldn't find the column 'wave' in the webtrack data", call. = FALSE)
        }
    }

    summary <- aggregate(x = list(temp = wt[[timeframe]]), by = list(panelist_id = wt$panelist_id), FUN = function(x) length(unique(x)))

    names(summary)[which(names(summary) == "temp")] <- timeframe_var

    return(summary)
}
