test_that("add duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt)
  expect_true("duration" %in% names(wt))
})

test_that("aggregate duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt)
  wt <- extract_domain(wt)
  expect_no_error(aggregate_duration(wt[1:10, ]))
})

test_that("extract_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  expect_true("domain" %in% names(wt))
})

test_that("drop_query", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
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

test_that("classify_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt <- add_duration(wt)
  wt <- classify_domains(wt)
  expect_true("type" %in% names(wt))
  expect_true("prev_type" %in% names(wt))
})

test_that("classify_domain errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt <- add_duration(wt)
  expect_error(classify_domains(wt,domain_classes = data.frame(a=5,b=6)))
  expect_error(classify_domains(wt,domain_classes = data.table::data.table(a=5,b=6)))
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
