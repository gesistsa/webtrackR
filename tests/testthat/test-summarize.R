test_that("sum_visits", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- extract_domain(wt)
  wt[,google:=ifelse(domain == "google.com", 1, 0)]
  wt[,search:=ifelse(grepl("search", url), 1, 0)]
  wt_sum <- sum_visits(wt)
  # expect_no_error(sum_visits(wt)) # expect_no_error not found - package version?
  expect_true("n_visits" %in% names(wt_sum))
  expect_true("panelist_id" %in% names(wt_sum))
  # expect_true(sum_visits(wt), is.null(timeframe_var)) # this does not work, I assume because timeframe_var is not carried outside function
  # also add other cases for timeframe_var
  # check whether grouping variables in wt_sum
})

test_that("sum_visits errors", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- extract_domain(wt)
  wt[,google:=ifelse(domain == "google.com", 1, 0)]
  wt[,search:=ifelse(grepl("search", url), 1, 0)]
  expect_error(sum_visits(wt, timeframe = "something"))
  expect_error(sum_visits(wt, timeframe = "wave"))
})
