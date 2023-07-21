test_that("add_duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt[, index := 1:.N, by=panelist_id]
  wt[, n := .N, by=panelist_id]
  wt[, device := ifelse(index < n/2, "desktop", "mobile")]
  wt[, index := NULL]
  wt[, n := NULL]
  wt_duration <- add_duration(wt)
  expect_true("duration" %in% names(wt_duration))
  # testing that duration variable is not NA
  expect_true(min(wt_duration$duration, na.rm = T) >= 0)
  # testing "device_switch_na = T"
  wt[, device_next := shift(device, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  expect_true(is.na(wt[is.na(device_next != device)][["duration"]][1]))
  # testing "last_replace_by = NA"
  wt_duration <- add_duration(wt[panelist_id == unique(wt$panelist_id)[1]],
                              cutoff = 10000, replace_by = 10000,
                              last_replace_by = NA)
  expect_true(is.na(wt[nrow(wt), "duration"]))
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
  wt_sum <- deduplicate(wt[1:100, ], method = "aggregate",
                        duration_var = "duration", keep_nvisits = TRUE)
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
  wt <- extract_domain(wt)
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
  test_url <- c("https://www.google.com/maps/@25.6726944,-80.4421392,19z")
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
  wt <- extract_domain(wt)
  code_urls <- c("https://invite.rmrsurveys.com/survey/selfserve/2252/1812586")
  wt <- create_urldummy(wt,dummy = code_urls, name = "test_dummy")
  expect_true(wt$test_dummy[1])
})

test_that("panelist_data", {
  data("testdt_tracking")
  data("testdt_survey_w")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt,testdt_survey_w)
  expect_true("leftright"%in%names(wt))
})

test_that("panelist_data errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt,testdt_survey_w)
  expect_error(add_panelist_data(wt,"test"))
})
