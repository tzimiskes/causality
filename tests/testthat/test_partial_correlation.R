library(causality)
context("1st order partial correlation tests")

test_that("length testing works", {
  x <- rnorm(1000)
  y <- rnorm(1023)
  z <- runif(1004)
  expect_error(partial_correlation(x, y, z))
  y <- rnorm(1000)
  expect_error(partial_correlation(x, y, z))
})

test_that("NAs are dectected", {
  x <- rnorm(1000)
  y <- rnorm(1000)
  z <- runif(1000)
  na <- x[sample(c(F,T), 1000, replace = T)]

  expect_error(partial_correlation(na, y, z))
  expect_error(partial_correlation(x, na, z))
  expect_error(partial_correlation(x, y, na))
})

test_that("coercison works", {
  x <- rnorm(1000)
  int <- seq(1:1000)
  y <- rnorm(1000)
  z <- runif(1000)
  # check to see if integer coercing works correctly
  expect_message(partial_correlation(int, y, z))
  expect_message(partial_correlation(x, int, z))
  expect_message(partial_correlation(x, y, int))

  ordered <- as.ordered(int)
  expect_message(partial_correlation(ordered, y, z))
  expect_message(partial_correlation(x, ordered, z))
  expect_message(partial_correlation(x, y, ordered))

  factor <- as.factor(int)
  expect_error(partial_correlation(factor, y, z))
  expect_error(partial_correlation(x, factor, z))
  expect_error(partial_correlation(x, y, factor))
})

test_that("constant vectors throw errors", {
  x <- rnorm(1000)
  const <- rep(0,1000)
  y <- rnorm(1000)
  z <- runif(1000)
  # check to see if integer coercing works correctly
  expect_error(partial_correlation(const, y, z))
  expect_error(partial_correlation(x, const, z))
  expect_error(partial_correlation(x, y, const))
})

#TODO replace this with statisitcal independce tests once fisher z transform is implemented
test_that(" the partial correlations make sense", {
  set.seed(1)
  n <- 1e6
  x <- rnorm(n, sd = .1)
  y <- rnorm(n, sd = .1)
  z <- rnorm(n, sd = .1)

  expect_equal(partial_correlation(x,y,z), 0, tolerance = .01)

  y <- x + rnorm(n, sd = .05)
  expect_equal(partial_correlation(x, y, z), correlation(x,y), tolerance = .01)
  # x -> y -> z
  z <- y + rnorm(n, sd = .05)
  expect_equal(partial_correlation(x,z,y), 0, tolerance = .01)
  # x-> y <- z
  z <- rnorm(n, sd = .1)
  y <- x + z + rnorm(n, sd = .05)
  expect_gt(abs(partial_correlation(x,y,z)), 0)

  y <- rnorm(n, sd = .1)
  x <- y + rnorm(n, sd = .05)
  z <- y + rnorm(n, sd = .05)
  expect_equal(partial_correlation(x,z,y), 0, tolerance = .01)
})

