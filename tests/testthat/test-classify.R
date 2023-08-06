# test_that("classify_domain", {
#   data("testdt_tracking")
#   wt <- as.wt_dt(testdt_tracking)
#   wt <- extract_domain(wt)
#   wt <- add_duration(wt)
#   wt <- classify_domains(wt)
#   expect_true("type" %in% names(wt))
#   expect_true("prev_type" %in% names(wt))
# })

# test_that("classify_domain errors", {
#   data("testdt_tracking")
#   wt <- as.wt_dt(testdt_tracking)
#   wt <- extract_domain(wt)
#   wt <- add_duration(wt)
#   expect_error(classify_domains(wt,domain_classes = data.frame(a=5,b=6)))
#   expect_error(classify_domains(wt,domain_classes = data.table::data.table(a=5,b=6)))
# })

test_that("classify_visits", {
  data("testdt_tracking")
  data("domain_list")
  wt <- as.wt_dt(testdt_tracking)
  wt_domains <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
  wt_classes <- classify_visits(wt_domains, classes = domain_list, match_by = "domain")
  expect_true("type" %in% names(wt_classes))
})


test_that("classify_visits errors", {
  data("testdt_tracking")
  wt <- as.wt_dt(testdt_tracking)
  wt_domains <- suppressWarnings(extract_domain(wt, drop_na = FALSE))
  expect_error(classify_visits(wt_domains, classes = data.frame(a = 5, b = 6)))
  expect_error(classify_visits(wt_domains, classes = domain_list, match_by = "not_a_value"))
  expect_error(classify_visits(wt_domains, classes = domain_list, match_by = "host"))
  expect_error(classify_visits(wt_domains, classes = domain_list, match_by = "regex"))
  expect_error(classify_visits(wt_domains, classes = domain_list, match_by = "regex", regex_on = "not_a_variable"))
  expect_error(classify_visits(wt_domains,
    classes = domain_list, match_by = "domain",
    return_rows_by = "type", return_rows_val = NULL
  ))
})
