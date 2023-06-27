test_that("add duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt)
  expect_true("duration"%in% names(wt))
})

test_that("aggregate duration", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_duration(wt)
  wt <- extract_domain(wt)
  expect_no_error(aggregate_duration(wt[1:10,]))
})

test_that("extract_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  expect_true("domain"%in% names(wt))
})

test_that("classify_domain", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- extract_domain(wt)
  wt <- add_duration(wt)
  wt <- classify_domains(wt)
  expect_true("type"%in% names(wt))
  expect_true("prev_type"%in% names(wt))
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
  code_urls <- c("Ccj4QELzbJe6.com/FrKrkvugBVJWwfSobV")
  wt <- create_urldummy(wt,dummy = code_urls, name = "test_dummy")
  expect_true(wt$test_dummy[1])
})

test_that("panelist_data", {
  data("testdt_tracking")
  data("test_survey")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt,test_survey)
  expect_true("leftright"%in%names(wt))
})

test_that("panelist_data errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- add_panelist_data(wt,test_survey)
  expect_error(add_panelist_data(wt,"test"))
})
