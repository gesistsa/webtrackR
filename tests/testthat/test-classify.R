test_that("classify_visits", {
  data("testdt_tracking")
  data("domain_list")
  wt <- as.wt_dt(testdt_tracking)
  wt <- suppressWarnings(extract_domain(wt, drop_na = F))
  wt_classes <- classify_visits(wt, classes = domain_list, match_by = "domain")
  # test existence of columns
  expect_true("type" %in% names(wt_classes))
  # test number of rows
  expect_true(nrow(wt_classes) == nrow(wt))
  # test number of rows when filtering to class
  return_rows_val_param <- "search"
  wt_classes <- classify_visits(wt, classes = domain_list, match_by = "domain",
                                return_rows_by = "type", return_rows_val = return_rows_val_param)
  expect_true(nrow(wt_classes) < nrow(wt))
  expect_true(length(table(wt_classes$type)) == length(return_rows_val_param))
})

test_that("classify_visits errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  data("domain_list")
  # test error when wrong value given to match_by
  expect_error(classify_visits(wt, classes = domain_list, match_by = "not_a_value"))
  # test error when classes is not a data.table
  class(domain_list) <- "data.frame"
  expect_error(classify_visits(wt, classes = domain_list, match_by = "domain"))
  # test errors when wt does not have column to match
  data("domain_list")
  expect_error(classify_visits(wt, classes = domain_list, match_by = "domain"))
  expect_error(classify_visits(wt, classes = domain_list, match_by = "host"))
  expect_error(classify_visits(wt, classes = domain_list, match_by = "regex"))
  expect_error(classify_visits(wt, classes = domain_list,
                               match_by = "regex", regex_on = "not_a_variable"))
  setnames(wt, "url", "url_other")
  expect_error(classify_visits(wt, classes = domain_list, match_by = "regex", regex_on = "domain"))
  # test errors when classes does not have column to match
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- suppressWarnings(extract_domain(wt, drop_na = F))
  wt <- suppressWarnings(extract_host(wt, drop_na = F))
  setnames(wt, "domain", "domain_other")
  expect_error(classify_visits(wt, classes = domain_list, match_by = "domain"))
  expect_error(classify_visits(wt, classes = domain_list, match_by = "host"))
  expect_error(classify_visits(wt, classes = domain_list, match_by = "regex", regex_on = "domain"))
  # test error when return_rows_by but not return_rows_val specified
  expect_error(classify_visits(wt, classes = domain_list, match_by = "domain", return_rows_by = "type"))
})

test_that("classify_visits testdt_specific", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt <- suppressWarnings(extract_domain(wt, drop_na = F))
  # test number of cases in each categories when classified via domain
  wt_classes <- classify_visits(wt, classes = domain_list, match_by = "domain")
  expect_true(table(wt_classes$type)["ebay"] == 39)
  expect_true(table(wt_classes$type)["facebook"] == 1374)
  expect_true(table(wt_classes$type)["news"] == 300)
  expect_true(table(wt_classes$type)["portal"] == 623)
  expect_true(table(wt_classes$type)["search"] == 2795)
  expect_true(table(wt_classes$type)["twitter"] == 1910)
  expect_true(table(wt_classes$type)["twitter"] == 1910)
  expect_true(sum(is.na(wt_classes$type)) == 42571)
  # test number of cases in each categories when classified via host
  # (this should give much fewer cases, as list is on domain not host level)
  wt <- suppressWarnings(extract_host(wt, drop_na = F))
  domain_list[,host:=domain]
  wt_classes <- classify_visits(wt, classes = domain_list, match_by = "host")
  expect_true(table(wt_classes$type)["news"] == 6)
  expect_true(table(wt_classes$type)["newsportals"] == 214)
  expect_true(table(wt_classes$type)["twitter"] == 1908)
  expect_true(sum(is.na(wt_classes$type)) == 47484)
  # test number of cases when classified via regex
  regex_list <- domain_list[type == "facebook"]
  wt_classes <- classify_visits(wt, classes = regex_list, match_by = "regex", regex_on = "domain")
  expect_true(table(wt_classes$type)["facebook"] == 1374)
  expect_true(sum(is.na(wt_classes$type)) == 48238)
  # test number of cases when only rows classified as "search" returned
  expect_true(nrow(classify_visits(wt, classes = domain_list, match_by = "domain",
                                   return_rows_by = "type", return_rows_val = "search")) == 2795)

})
