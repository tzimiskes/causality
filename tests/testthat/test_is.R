library(causality)
context("Test is.foo correctness")

test_that("is.cgraph works", {
  graph1 <- 1
  class(graph1) <-.CGRAPH_CLASS
  graph2 <- 2
  class(graph2) <- c("apple", .DAG_CLASS)

  graph3 <- 3
  class(graph3) <- c("apple", "battle", .PDAG_CLASS)
  expect_true(is.cgraph(graph1))
  expect_true(is.cgraph(graph2))
  expect_true(is.cgraph(graph3))
})

test_that("is.dag works", {
  graph1 <- 1
  class(graph1) <- .CGRAPH_CLASS
  graph2 <- 2
  class(graph2) <- .DAG_CLASS
  graph3 <- 3
  class(graph3) <- c("apple", "battle", .DAG_CLASS, .PDAG_CLASS)
  expect_false(is.dag(graph1))
  expect_true(is.dag(graph2))
  expect_false(is.dag(graph3))
})

test_that("is.pattern works", {
  graph1 <- 1
  class(graph1) <- .CGRAPH_CLASS
  graph2 <- 2
  class(graph2) <- c("apple", .PATTERN_CLASS)
  graph3 <- 3
  class(graph3) <- .PATTERN_CLASS
  expect_false(is.pattern(graph1))
  expect_false(is.pattern(graph2))
  expect_true(is.pattern(graph3))
})

test_that("is.pdag works", {
  graph1 <- 1
  class(graph1) <- .CGRAPH_CLASS
  graph2 <- 2
  class(graph2) <- c("apple", "battle", .DAG_CLASS)
  graph3 <- 3
  class(graph3) <- .PDAG_CLASS
  graph4 <- 3
  class(graph4) <- c(.PDAG_CLASS, "potato")
  expect_false(is.pdag(graph1))
  expect_false(is.pdag(graph2))
  expect_true(is.pdag(graph3))
  expect_false(is.pdag(graph4))
})


test_that("is.pag works", {
  graph1 <- 1
  class(graph1) <- .PAG_CLASS
  graph2 <- 2
  class(graph2) <- c("apple", "battle", .PAG_CLASS)
  graph3 <- 3
  class(graph3) <- .CGRAPH_CLASS
  graph4 <- 3
  expect_true(is.pag(graph1))
  expect_false(is.pag(graph2))
  expect_false(is.pag(graph3))
})