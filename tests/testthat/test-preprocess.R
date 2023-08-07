test_that("add_duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_duration <- add_duration(wt, cutoff = 300, replace_by = NA)
  # test that variables in result
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

test_that("add_session", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_session <- add_session(wt, cutoff = 1800)
  # test that variables in result
  expect_true("session" %in% names(wt_session))
  # test that session variable always positive
  expect_true(min(wt_session$session, na.rm = T) >= 1)
  # test that next session is only smaller than session when switch to new panelist
  wt_session[, next_session:= shift(session, n = 1, type = "lead", fill = NA), by = "panelist_id"]
  expect_true(nrow(wt_session[session > next_session]) <=
                length(unique(wt_session$panelist_id)))
})

test_that("add_session errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  # no cutoff specified
  expect_error(add_session(wt))
})

test_that("add_session testdt_specific", {
  options(digits = 22)
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_session <- add_session(wt, cutoff = 1800)
  expect_true(wt_session[panelist_id == "AiDS4k1rQZ"][.N, "session"] == 123)
})

test_that("deduplicate", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt, cutoff = 300, replace_by = 300)
  # test that variables in result
  wt_dedup <- deduplicate(wt, method = "flag")
  expect_true("duplicate" %in% names(wt_dedup))
  wt_dedup <- deduplicate(wt, method = "drop")
  expect_true(!"duplicate" %in% names(wt_dedup))
  wt_dedup <- deduplicate(wt, method = "aggregate", keep_nvisits = TRUE)
  expect_true("visits" %in% names(wt_dedup))
})

test_that("deduplicate errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt, cutoff = 300, replace_by = 300)
  expect_error(deduplicate(wt, method = "aggregate", duration_var = "not_a_variable"))
  expect_error(deduplicate(wt, method = "flag", within = NULL))
  expect_error(deduplicate(wt, method = "drop", within = NULL))
})

test_that("deduplicate testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt, cutoff = 300, replace_by = 300)
  wt_dedup <- deduplicate(wt, method = "drop")
  expect_true(nrow(wt_dedup) == 46574)
  wt_dedup <- deduplicate(wt, method = "flag")
  expect_true(sum(wt_dedup[, "duplicate"]) == 3038)
  wt_dedup <- deduplicate(wt, method = "aggregate")
  expect_true(nrow(wt_dedup) == 39540)
  wt_dedup <- deduplicate(wt, method = "aggregate", keep_nvisits = T)
  expect_true(max(wt_dedup[, "visits"]) == 608)
})

test_that("extract_host", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_host <- suppressWarnings(extract_host(wt))
  expect_true("host" %in% names(wt_host))
  wt[,other_url:=url]
  wt_host <- suppressWarnings(extract_host(wt, varname = "other_url"))
  expect_true("other_url_host" %in% names(wt_host))
})

test_that("extract_host errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_host(wt, varname = "not_a_variable"))
})

test_that("extract_host testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_host <- suppressWarnings(extract_host(wt, drop_na = TRUE))
  expect_true(wt_host[1,"host"] == "dkr1.ssisurveys.com")
  expect_true(nrow(wt_host) == 49583)
  wt_host <- suppressWarnings(extract_host(wt, drop_na = FALSE))
  expect_true(nrow(wt_host) == nrow(wt))
})

test_that("extract_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  # test existence of variables
  wt_domain <- suppressWarnings(extract_domain(wt))
  expect_true("domain" %in% names(wt_domain))
  wt[,other_url:=url]
  wt_domain <- suppressWarnings(extract_domain(wt, varname = "other_url"))
  expect_true("other_url_domain" %in% names(wt_domain))
  wt_domain <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
  # test domain composition
  wt_domain[, tmp_host := urltools::domain(gsub("@", "%40", url))]
  wt_domain[, tmp_suffix := urltools::suffix_extract(tmp_host)[["suffix"]]]
  wt_domain[, tmp_domain_name := urltools::suffix_extract(tmp_host)[["domain"]]]
  expect_true(is.na(wt_domain[is.na(tmp_suffix), "domain"][1]))
  expect_true(wt_domain[!is.na(tmp_suffix) & is.na(tmp_domain_name), "domain"] ==
                wt_domain[!is.na(tmp_suffix) & is.na(tmp_domain_name), "tmp_suffix"])
})

test_that("extract_domain errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_domain(wt, varname = "not_a_variable"))
})

test_that("extract_domain testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_domain <- suppressWarnings(extract_domain(wt, drop_na = TRUE))
  expect_true(wt_domain[1,"domain"] == "ssisurveys.com")
  expect_true(nrow(wt_domain) == 49451)
  wt_domain <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
  expect_true(nrow(wt_domain) == nrow(wt))
})

test_that("extract_path", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_path <- extract_path(wt)
  expect_true("path" %in% names(wt_path))
  wt[,other_url:=url]
  wt_host <- extract_path(wt, varname = "other_url")
  expect_true("other_url_path" %in% names(wt_host))
})

test_that("extract_path errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_error(extract_path(wt, varname = "not_a_variable"))
})

test_that("extract_path testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_path <- extract_path(wt)
  expect_true(wt_path[1, "path"] == "tzktsxomta")
  expect_true(is.na(wt_path[url == "https://www.youtube.com/", "path"][1]))
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
  wt <- as.wt_dt(testdt_tracking[1])
  wt_title <- add_title(wt)
  expect_true("title" %in% names(wt_title))
})

test_that("add_title testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking[1])
  wt_title <- add_title(wt)
  expect_true(is.na(wt_title[,"title"]))
  wt[, url:="google.com"]
  wt_title <- add_title(wt)
  expect_true(wt_title[,"title"] == "Google")
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
