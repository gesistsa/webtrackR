test_that("sum_visits", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- extract_domain(wt)
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    # test existence of columns
    wt_sum <- sum_visits(wt)
    expect_true("n_visits" %in% names(wt_sum))
    expect_true("panelist_id" %in% names(wt_sum))
    expect_true(length(names(wt_sum)) == 2)
    # test correct number of panelists
    expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
    # test correct number of visits
    expect_true(nrow(wt) == sum(wt_sum$n_visits, na.rm = TRUE))
    # test existence of columns when timeframe specified
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
    # test number of classes visits
    wt_sum_google <- sum_visits(wt, visit_class = "google")
    expect_true("n_visits_google_0" %in% names(wt_sum_google))
    expect_true("n_visits_google_1" %in% names(wt_sum_google))
    expect_true(sum(wt_sum_google$n_visits_google_1, na.rm = T) == sum(wt$google, na.rm = T))
})

test_that("sum_visits errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- extract_domain(wt)
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    expect_error(sum_visits(wt, timeframe = "not_a_variable"))
    wt[["wave"]] <- NULL
    expect_error(sum_visits(wt, timeframe = "wave"))
    expect_error(sum_visits(wt, visit_class = "some_class_not_in_wt"))
})

test_that("sum_visits testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- extract_domain(wt)
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    wt_sum <- sum_visits(wt)
    # test correct number of visits
    expect_true(sum(wt_sum$n_visits, na.rm = TRUE) == 49612)
    # test correct number of visits to Google
    wt_sum_google <- sum_visits(wt, visit_class = "google")
    expect_true(sum(wt_sum_google$n_visits_google_0, na.rm = TRUE) == 46897)
    expect_true(sum(wt_sum_google$n_visits_google_1, na.rm = TRUE) == 2554)
})

test_that("sum_durations", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- extract_domain(wt)
    wt <- add_duration(wt)
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    wt_sum <- sum_durations(wt)
    # test existence of columns
    expect_true("duration_visits" %in% names(wt_sum))
    expect_true("panelist_id" %in% names(wt_sum))
    expect_true(length(names(wt_sum)) == 2)
    # test correct number of panelists
    expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
    # test correct duration of visits
    expect_true(sum(wt$duration, na.rm = TRUE) == sum(wt_sum$duration_visits, na.rm = TRUE))
    # test existence of columns when timeframe specified
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
    # test number of classes visits durations
    wt_sum_google <- sum_durations(wt, visit_class = "google")
    expect_true("duration_visits_google_0" %in% names(wt_sum_google))
    expect_true("duration_visits_google_1" %in% names(wt_sum_google))
    expect_true(sum(wt_sum_google$duration_visits_google_1, na.rm = TRUE) == sum(wt$duration[wt$google == 1], na.rm = TRUE))
})

test_that("sum_durations errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- suppressWarnings(extract_domain(wt))
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    wt$search <- ifelse(grepl("search", wt$url), 1, 0)
    expect_error(sum_durations(wt, duration_var = "not_a_variable"))
    expect_error(sum_durations(wt, timeframe = "not_a_variable"))
    wt[["wave"]] <- NULL
    expect_error(sum_durations(wt, timeframe = "wave"))
    expect_error(sum_durations(wt, visit_class = "some_class_not_in_wt"))
})

test_that("sum_durations testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt <- suppressWarnings(extract_domain(wt))
    wt <- add_duration(wt)
    wt$google <- ifelse(wt$domain == "google.com", 1, 0)
    wt_sum <- sum_durations(wt)
    # test correct number of visits
    expect_true(round(sum(wt_sum$duration_visits, na.rm = TRUE), 0) == round(sum(wt$duration, na.rm = TRUE), 0))
    # test correct number of visits to Google
    wt_sum_google <- sum_durations(wt, visit_class = "google")
    expect_true(round(sum(wt_sum_google$duration_visits_google_0, na.rm = TRUE), 0) == 1119003)
    expect_true(round(sum(wt_sum_google$duration_visits_google_1, na.rm = TRUE), 0) == round(sum(wt$duration[wt$google == 1], na.rm = TRUE), 0))
})

test_that("sum_activity", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt_sum <- sum_activity(wt)
    # test existence of columns
    expect_true("active_dates" %in% names(wt_sum))
    expect_true("panelist_id" %in% names(wt_sum))
    # test correct number of panelists
    expect_true(length(unique(wt_sum$panelist_id)) == length(unique(wt$panelist_id)))
    # test existence of columns when timeframe specified
    wt_sum_week <- sum_activity(wt, timeframe = "week")
    expect_true("active_weeks" %in% names(wt_sum_week))
    wt_sum_month <- sum_activity(wt, timeframe = "month")
    expect_true("active_months" %in% names(wt_sum_month))
    wt_sum_year <- sum_activity(wt, timeframe = "year")
    expect_true("active_years" %in% names(wt_sum_year))
    wt_sum_wave <- sum_activity(wt, timeframe = "wave")
    expect_true("active_waves" %in% names(wt_sum_wave))
})
test_that("sum_activity errors", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    expect_error(sum_activity(wt, timeframe = "not_a_variable"))
    wt[["wave"]] <- NULL
    expect_error(sum_activity(wt, timeframe = "wave"))
})

test_that("sum_activity testdt_specific", {
    data("testdt_tracking")
    wt <- as.wt_dt(testdt_tracking)
    wt$timestamp <- wt$timestamp + 60 * 60 * 60 + 2
    wt_sum <- sum_activity(wt)
    # test correct number of active days
    expect_equal(sum(wt_sum$active_dates, na.rm = TRUE), 182)
})
