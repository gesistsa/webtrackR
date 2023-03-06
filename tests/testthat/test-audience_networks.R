test_that("audience_incidence", {
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- add_duration(wt)
  wt <- extract_domain(wt)
  A <- audience_incidence(wt)
  expect_equal(dim(A)[1],766)
  expect_equal(dim(A)[2],9)
})

test_that("audience_networks", {
  library(igraph)
  data("test_data")
  wt <- as.wt_dt(test_data)
  wt <- add_duration(wt)
  wt <- extract_domain(wt)
  pmi <- audience_network(wt, type = "pmi", cutoff = 120)
  expect_equal(ecount(pmi),16583)
})
