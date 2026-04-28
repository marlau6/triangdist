library(testthat)
library(triangdist)

test_that("dtriang: parameter validation and error messages", {
  # Coverage of the stop() conditions

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

test_that("ptriang: parameter validation and error messages", {
  # Coverage of the stop() conditions

  e1 <- "Invalid parameters: min > max"
  e2 <- "Invalid parameters: mode < min"
  e3 <- "Invalid parameters: mode > max"

  expect_error(ptriang(0.5, min = 10, max = 5, mode = 7), e1)
  expect_error(ptriang(0.5, min = 2, max = 10, mode = 1), e2)
  expect_error(ptriang(0.5, min = 0, max = 5, mode = 6), e3)
})

test_that("ptriang: cumulative distribution values for different segments", {
  # Scenario: a=0, b=10, c=2
  a <- 0
  b <- 10
  c <- 2

  # 1. Below 'min' (initializes Y as 0)
  expect_equal(ptriang(-1, a, b, c), 0)

  # 2. Case 'a_to_c': q = 1
  expect_equal(ptriang(1, a, b, c), 0.05)

  # 3. Case 'x_eq_c': q = 2 (The Mode)
  expect_equal(ptriang(2, a, b, c), 0.2)

  # 4. Case 'c_to_b': q = 6
  expect_equal(ptriang(6, a, b, c), 0.8)

  # 5. Case 'x_bgr_b': q = 11 (Biger than max)
  expect_equal(ptriang(11, a, b, c), 1)
})

test_that("ptriang: vectorization and recycling rules", {
  # Input vector
  q_vec <- c(1, 2, 6)
  expected_res <- c(0.05, 0.2, 0.8)

  expect_length(ptriang(q_vec, 0, 10, 2), 3)
  expect_equal(ptriang(q_vec, 0, 10, 2), expected_res)

  # Parameter recycling (vectorized min)
  expect_length(ptriang(0.5, min = c(0, 0), max = 1, mode = 0.5), 2)
})

test_that("qtriang: parameter validation and error messages", {
  # Coverage of the stop() conditions

  e1 <- "Invalid parameters: min > max"
  e2 <- "Invalid parameters: mode < min"
  e3 <- "Invalid parameters: mode > max"
  e4 <- "Invalid parameters: p < 0"
  e5 <- "Invalid parameters: p > 1"

  expect_error(qtriang(0.5, min = 10, max = 5, mode = 7), e1)
  expect_error(qtriang(0.5, min = 2, max = 10, mode = 1), e2)
  expect_error(qtriang(0.5, min = 0, max = 5, mode = 6), e3)
  expect_error(qtriang(-0.1, min = 0, max = 10, mode = 5), e4)
  expect_error(qtriang(1.1, min = 0, max = 10, mode = 5), e5)
})

test_that("qtriang: quantile values for different segments", {
  # Scenario: a=0, b=10, c=2
  a <- 0
  b <- 10
  c <- 2
  p_mode <- 0.2

  # 1. Minimum boundary (p = 0)
  expect_equal(qtriang(0, a, b, c), a)

  # 2. Lower branch: p = 0.05
  expect_equal(qtriang(0.05, a, b, c), 1)

  # 3. At the mode: p = 0.2
  expect_equal(qtriang(0.2, a, b, c), c)

  # 4. Upper branch: p = 0.8
  expect_equal(qtriang(0.8, a, b, c), 6)

  # 5. Maximum boundary (p = 1)
  expect_equal(qtriang(1, a, b, c), b)
})

test_that("qtriang: vectorization and recycling rules", {
  # Input vector
  p_vec <- c(0.05, 0.2, 0.8)
  expected_res <- c(1, 2, 6)

  expect_length(qtriang(p_vec, 0, 10, 2), 3)
  expect_equal(qtriang(p_vec, 0, 10, 2), expected_res)

  # Parameter recycling (vectorized min)
  expect_length(qtriang(0.5, min = c(0, 0), max = 10, mode = 5), 2)
})

