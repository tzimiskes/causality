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

test_that("NAs are detected", {
  x <- rnorm(1000)
  y <- rnorm(1000)
  z <- runif(1000)
  na <- x[sample(c(F, T), 1000, replace = T)]

  expect_error(partial_correlation(na, y, z))
  expect_error(partial_correlation(x, na, z))
  expect_error(partial_correlation(x, y, na))
})

test_that("coercison works", {
  x <- rnorm(1000)
  int <- seq(1:1000)
  y <- rnorm(1000)
  z <- runif(1000)
  # check to see if coercing integers works correctly
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
  const <- rep(0, 1000)
  y <- rnorm(1000)
  z <- runif(1000)

  expect_error(partial_correlation(const, y, z))
  expect_error(partial_correlation(x, const, z))
  expect_error(partial_correlation(x, y, const))
})

test_that("the partial correlations make sense", {
  set.seed(1)
  n <- 1e5
  x <- rnorm(n, sd = .1)
  y <- rnorm(n, sd = .1)
  z <- rnorm(n, sd = .1)


  r_hat <- partial_correlation(x,y,z)
  z_score <- fisher_z_score(r_hat, n)
  expect_true(test_fisher_independence(z_score, .05))

  # x -> y
  # rho_{x,y|z} = rho(x,y) != 0
  y <- x + rnorm(n, sd = .05)
  # test rho_{x,y|z} = rho_{x,y}
  expect_equal(partial_correlation(x, y, z),
               .correlation(x, y), tolerance = 1e-5)
  # test rho{x,y|z} != 0
  r_hat <- partial_correlation(x, y, z)
  z_score <- fisher_z_score(r_hat, n)
  expect_false(test_fisher_independence(z_score, .01))
  # x -> y -> z
  z <- y + rnorm(n, sd = .05)
  r_hat <- partial_correlation(x, z, y)
  z_score <- fisher_z_score(r_hat, n)
  expect_true(test_fisher_independence(z_score, .01))
  # x-> y <- z
  z <- rnorm(n, sd = .1)
  y <- x + z + rnorm(n, sd = .05)
  r_hat <- partial_correlation(x, z, y)

  z_score <- fisher_z_score(r_hat, n)
  expect_false(test_fisher_independence(z_score, .01))
  # z <- y -> x
  y <- rnorm(n, sd = .1)
  x <- y + rnorm(n, sd = .05)
  z <- y + rnorm(n, sd = .05)
  r_hat <- partial_correlation(x, z, y)
  z_score <- fisher_z_score(r_hat, n)
  expect_true(test_fisher_independence(z_score, .01))
})

