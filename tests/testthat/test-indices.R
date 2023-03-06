test_that("isolation index works", {
  left <-  c(5,5,0,0)
  right <- c(0,0,5,5)
  expect_equal(isolation_index(left,right),1)
  left <-  c(5,5,5,5)
  right <- c(5,5,5,5)
  expect_equal(isolation_index(left,right),0)
})

test_that("isolation index errors", {
  left <-  c(5,5,0)
  right <- c(0,0,5,5)
  expect_error(isolation_index(left,right))
})
