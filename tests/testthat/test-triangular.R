library(testthat)
library(triangdist)

test_that("dtriang: parameter validation and error messages", {
  # These tests ensure 100% coverage of your stop() conditions

  e1 <- "Invalid parameters: min > max"
  e2 <- "Invalid parameters: mode < min"
  e3 <- "Invalid parameters: mode > max"

  expect_error(dtriang(0.5, min = 10, max = 5, mode = 7), e1)
  expect_error(dtriang(0.5, min = 2, max = 10, mode = 1), e2)
  expect_error(dtriang(0.5, min = 0, max = 5, mode = 6), e3)
})

test_that("dtriang: density values for different segments", {
  # Scenario: a=0, b=10, c=5. Max height h = 2/(10-0) = 0.2
  a <- 0
  b <- 10
  c <- 5
  h <- 0.2

  # Increasing slope (a_to_c)
  expect_equal(dtriang(2.5, a, b, c), 0.1)

  # At the mode (c_eq_x)
  expect_equal(dtriang(5, a, b, c), 0.2)

  # Decreasing slope (c_to_b)
  expect_equal(dtriang(7.5, a, b, c), 0.1)

  # Outside boundaries (initializes y as 0)
  expect_equal(dtriang(-1, a, b, c), 0)
  expect_equal(dtriang(11, a, b, c), 0)
})

test_that("dtriang: vectorization and recycling rules", {
  # Input vector
  x_vec <- c(2.5, 5, 7.5)
  expect_length(dtriang(x_vec, 0, 10, 5), 3)
  expect_equal(dtriang(x_vec, 0, 10, 5), c(0.1, 0.2, 0.1))

  # Parameter recycling (vectorized min)
  expect_length(dtriang(5, min = c(0, 0), max = 10, mode = 5), 2)
})
