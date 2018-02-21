library(causality)
context("Test is.foo correctness")

test_that("is.cgraph works", {
  graph1 <- 1
  class(graph1) <- "cgraph"
  graph2 <- 2
  class(graph2) <- c("apple", "battle", "dag","cgraph")

  graph3 <- 3
  class(graph3) <- c("apple", "battle", "dag","pdag")
  expect_true(is.cgraph(graph1))
  expect_true(is.cgraph(graph2))
  expect_false(is.cgraph(graph3))
})

test_that("is.dag works", {
  graph1 <- 1
  class(graph1) <- "cgraph"
  graph2 <- 2
  class(graph2) <- c("apple", "battle", "dag","cgraph")
  graph3 <- 3
  class(graph3) <- c("apple", "battle", "dag","pdag")
  expect_false(is.dag(graph1))
  expect_true(is.dag(graph2))
  expect_true(is.dag(graph3))
})

test_that("is.pattern works", {
  graph1 <- 1
  class(graph1) <- "cgraph"
  graph2 <- 2
  class(graph2) <- c("apple", "battle", "dag","cgraph")
  graph3 <- 3
  class(graph3) <- c("apple", "battle", "dag","pdag")
  graph4 <- 3
  class(graph4) <- c("apple", "battle", "dag","pdag", "pattern")
  expect_false(is.pattern(graph1))
  expect_false(is.pattern(graph2))
  expect_false(is.pattern(graph3))
  expect_true(is.pattern(graph4))
})

test_that("is.pdag works", {
  graph1 <- 1
  class(graph1) <- "cgraph"
  graph2 <- 2
  class(graph2) <- c("apple", "battle", "dag","cgraph")
  graph3 <- 3
  class(graph3) <- c("apple", "battle", "dag","pdag")
  graph4 <- 3
  class(graph4) <- c("apple", "battle", "dag","pdag", "pattern")
  expect_false(is.pdag(graph1))
  expect_false(is.pdag(graph2))
  expect_true(is.pdag(graph3))
  expect_true(is.pdag(graph4))
})


test_that("is.pag works", {
  graph1 <- 1
  class(graph1) <- c("pag", "cgraph")
  graph2 <- 2
  class(graph2) <- c("apple", "battle", "dag","cgraph")
  graph3 <- 3
  class(graph3) <- c("apple", "battle", "pag", "pdag")
  graph4 <- 3
  class(graph4) <- c("apple", "battle", "dag","pdag", "pattern")
  expect_true(is.pag(graph1))
  expect_false(is.pag(graph2))
  expect_true(is.pag(graph3))
  expect_false(is.pag(graph4))
})