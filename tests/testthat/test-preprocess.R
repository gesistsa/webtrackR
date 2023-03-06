test_that("add duration", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- add_duration(wt)
  expect_true("duration"%in% names(wt))
})

test_that("extract_domain", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- extract_domain(wt)
  expect_true("domain"%in% names(wt))
})

test_that("classify_domain", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- extract_domain(wt)
  wt <- add_duration(wt)
  wt <- classify_domains(wt)
  expect_true("type"%in% names(wt))
  expect_true("prev_type"%in% names(wt))
})

test_that("urldummy", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- extract_domain(wt)
  code_urls <- c("Ccj4QELzbJe6.com/FrKrkvugBVJWwfSobV")
  wt <- create_urldummy(wt,dummy = code_urls, name = "test_dummy")
  expect_true(wt$test_dummy[1])
})

