test_that("as.wt_dt", {
  data("testdt_tracking")
  expect_no_error(as.wt_dt(testdt_tracking))
  names(testdt_tracking)[1] <- "wrong"
  expect_error(as.wt_dt(testdt_tracking))
})

test_that("is.wt_dt", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  expect_true(is.wt_dt(wt))
  expect_false(is.wt_dt(testdt_tracking))
})

test_that("summary", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  sum <- utils::capture.output(summary(wt))
  expect_true(any(grepl("Overview",sum)))
  expect_true(any(grepl("DATA",sum)))
})

test_that("print", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  sum <- utils::capture.output(print(wt))
  expect_true(any(grepl("webtrack data",sum)))
})
