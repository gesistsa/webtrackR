test_that("add_duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_duration <- add_duration(wt, cutoff = 300, replace_by = NA)
  expect_true("duration" %in% names(wt_duration))
  # test that duration variable is not NA
  expect_true(min(wt_duration$duration, na.rm = T) >= 0)
  # test that no duration > cutoff
  expect_true(max(wt_duration$duration, na.rm = T) < 300)
  # test that last row for panelist is NA for default
  expect_true(is.na(wt_duration[panelist_id == "AiDS4k1rQZ"][.N, "duration"]))
  # test that last row for panelist is not NA
  wt_duration <- add_duration(wt, last_replace_by = 0)
  expect_true(wt_duration[panelist_id == "AiDS4k1rQZ"][.N, "duration"] == 0)
  # test device_switch_na
  wt_duration <- add_duration(wt, device_switch_na = T, device_var = "device")
  wt_duration[, device_next := shift(device, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  expect_true(is.na(wt_duration[device_next != device][["duration"]][1]))
})

test_that("add_duration testdt_specific", {
  options(digits = 22)
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_duration <- add_duration(wt)
  # test duration for first row
  expect_true(as.numeric(wt_duration[1, "duration"]) == 2.8580000400543212890625)
  # test total duration
  expect_true(sum(wt_duration[, "duration"], na.rm = T) == 1177364.354005098342896)
})

test_that("add_duration errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(add_duration(wt, replace_by = -1))
  expect_error(add_duration(wt, device_switch_na = T, device_var = NULL))
  expect_error(add_duration(wt, device_switch_na = T, device_var = "not_a_variable"))
})

test_that("deduplicate", {
  data("testdt_tracking")
  wt1 <- as.wt_dt(testdt_tracking)[1:1000] # revisit with new example data
  wt2 <- as.wt_dt(testdt_tracking)[1:1000]
  wt <- data.table::rbindlist(list(wt1, wt2))
  wt <- as.wt_dt(wt)
  wt_drop <- deduplicate(wt, method = "flag")
  expect_true("duplicate" %in% names(wt_drop))
  wt_keep <- deduplicate(wt, method = "drop")
  expect_true(!"duplicate" %in% names(wt_keep))
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt, replace_by = 300)
  wt_sum <- deduplicate(wt[1:100, ],
    method = "aggregate",
    duration_var = "duration", keep_nvisits = TRUE
  )
  expect_true("visits" %in% names(wt_sum))
})

test_that("deduplicate errors", {
  data("testdt_tracking")
  wt1 <- as.wt_dt(testdt_tracking)[1:1000] # revisit with new example data
  wt2 <- as.wt_dt(testdt_tracking)[1:1000]
  wt <- data.table::rbindlist(list(wt1, wt2))
  wt <- as.wt_dt(wt)
  expect_error(deduplicate(wt[1:100, ], method = "aggregate", duration_var = "not_a_variable"))
})

test_that("extract_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- suppressWarnings(extract_domain(wt))
  expect_true("domain" %in% names(wt))
})

test_that("extract_domain errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_domain(wt, varname = "not_a_variable"))
})

test_that("extract_host", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_host(wt)
  expect_true("host" %in% names(wt))
})

test_that("extract_host errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_host(wt, varname = "not_a_variable"))
})

test_that("extract_path", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_path(wt)
  expect_true("path" %in% names(wt))
})

test_that("extract_path errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_path(wt, varname = "not_a_variable"))
})

test_that("drop_query", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- drop_query(wt)
  expect_true("url_noquery" %in% names(wt))
  test_url <- c("https://dkr1.ssisurveys.com/tzktsxomta")
  expect_true(wt[url == test_url, url] == test_url)
})

test_that("add_title", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking[1:10])
  wt <- add_title(wt)
  expect_true("title" %in% names(wt))
})

test_that("add_referral", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_referral(wt, platform_domains = "facebook.com", patterns = "fbclid=")
  expect_true("referral" %in% names(wt))
  expect_true(!"domain_previous" %in% names(wt))
})

test_that("add_referral errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(add_referral(wt))
  expect_error(add_referral(wt, platform_domains = "facebook.com"))
  expect_error(add_referral(wt, platform_domains = c("facebook.com", "twitter.com"), pattern = "some"))
})

test_that("urldummy", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- suppressWarnings(extract_domain(wt))
  code_urls <- c("https://dkr1.ssisurveys.com/tzktsxomta")
  wt <- create_urldummy(wt, dummy = code_urls, name = "test_dummy")
  expect_true(wt$test_dummy[1])
})

test_that("panelist_data", {
  data("testdt_tracking")
  data("testdt_survey_w")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt, testdt_survey_w)
  expect_true("leftright" %in% names(wt))
})

test_that("panelist_data errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt, testdt_survey_w)
  expect_error(add_panelist_data(wt, "test"))
})
