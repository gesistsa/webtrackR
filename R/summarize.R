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
