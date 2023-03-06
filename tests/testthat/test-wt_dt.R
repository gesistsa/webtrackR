test_that("as.wt_dt", {
  data("test_data")
  expect_no_error(as.wt_dt(test_data))
  names(test_data)[1] <- "wrong"
  expect_error(as.wt_dt(test_data))
})

test_that("is.wt_dt", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  expect_true(is.wt_dt(wt))
  expect_false(is.wt_dt(test_data))
})
