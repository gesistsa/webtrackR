test_that("sum_visits", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt[,google := ifelse(domain == "google.com", 1, 0)]
  wt[,search := ifelse(grepl("search", url), 1, 0)]
  wt_sum <- sum_visits(wt)
  expect_true("n_visits" %in% names(wt_sum))
  expect_true("panelist_id" %in% names(wt_sum))
  expect_true(!"all" %in% names(wt_sum))
  expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
  expect_true(nrow(wt) == sum(wt_sum$n_visits, na.rm = T))
  wt_sum_date <- sum_visits(wt, timeframe = "date")
  expect_true("date" %in% names(wt_sum_date))
  wt_sum_week <- sum_visits(wt, timeframe = "week")
  expect_true("week" %in% names(wt_sum_week))
  wt_sum_month <- sum_visits(wt, timeframe = "month")
  expect_true("month" %in% names(wt_sum_month))
  wt_sum_year <- sum_visits(wt, timeframe = "year")
  expect_true("year" %in% names(wt_sum_year))
  wt_sum_wave <- sum_visits(wt, timeframe = "wave")
  expect_true("wave" %in% names(wt_sum_wave))
})

test_that("sum_visits errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt[,google := ifelse(domain == "google.com", 1, 0)]
  wt[,search := ifelse(grepl("search", url), 1, 0)]
  expect_error(sum_visits(wt, timeframe = "not_a_variable"))
  wt[["wave"]] <- NULL
  expect_error(sum_durations(wt, timeframe = "wave"))
})

test_that("sum_durations", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt[,google := ifelse(domain == "google.com", 1, 0)]
  wt[,search := ifelse(grepl("search", url), 1, 0)]
  wt_sum <- sum_durations(wt)
  expect_true("duration_visits" %in% names(wt_sum))
  expect_true("panelist_id" %in% names(wt_sum))
  expect_true(!"all" %in% names(wt_sum))
  expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
  expect_true(sum(wt$duration, na.rm = T) == sum(wt_sum$duration_visits, na.rm = T))
  wt_sum_date <- sum_durations(wt, timeframe = "date")
  expect_true("date" %in% names(wt_sum_date))
  wt_sum_week <- sum_durations(wt, timeframe = "week")
  expect_true("week" %in% names(wt_sum_week))
  wt_sum_month <- sum_durations(wt, timeframe = "month")
  expect_true("month" %in% names(wt_sum_month))
  wt_sum_year <- sum_durations(wt, timeframe = "year")
  expect_true("year" %in% names(wt_sum_year))
  wt_sum_wave <- sum_durations(wt, timeframe = "wave")
  expect_true("wave" %in% names(wt_sum_wave))
})
test_that("sum_durations errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt[,google := ifelse(domain == "google.com", 1, 0)]
  wt[,search := ifelse(grepl("search", url), 1, 0)]
  expect_error(sum_durations(wt, duration_var = "not_a_variable"))
  expect_error(sum_durations(wt, timeframe = "not_a_variable"))
  wt[["wave"]] <- NULL
  expect_error(sum_durations(wt, timeframe = "wave"))
})
test_that("sum_activity", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_sum <- sum_activity(wt)
  expect_true("active_dates" %in% names(wt_sum))
  expect_true("panelist_id" %in% names(wt_sum))
  expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
  wt_sum_week <- sum_activity(wt, timeframe = "week")
  expect_true("week" %in% names(wt_sum_week))
  wt_sum_month <- sum_activity(wt, timeframe = "month")
  expect_true("month" %in% names(wt_sum_month))
  wt_sum_year <- sum_activity(wt, timeframe = "year")
  expect_true("year" %in% names(wt_sum_year))
  wt_sum_wave <- sum_activity(wt, timeframe = "wave")
  expect_true("wave" %in% names(wt_sum_wave))
})
test_that("sum_activity errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(sum_activity(wt, timeframe = "not_a_variable"))
  wt[["wave"]] <- NULL
  expect_error(sum_activity(wt, timeframe = "wave"))
})

test_that("agg_duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt, replace_by = "cutoff")
  wt_sum <- agg_duration(wt[1:100, ], keep = TRUE)
  expect_true("visits" %in% names(wt_sum))
})
test_that("agg_duration errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(agg_duration(wt[1:100, ]))
  expect_error(agg_duration(wt[1:100, ], duration_var = "not_a_variable"))
})
