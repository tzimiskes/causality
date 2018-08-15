library(causality)

context("sort.casuality.graph works")

test_that("sort generates the correct topological sort", {
dag <- cgraph(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), edges =
                matrix(c("X4", "X5", "-->",
                         "X4", "X7", "-->",
                         "X4", "X1", "-->",
                         "X4", "X2", "-->",
                         "X4", "X6", "-->",
                         "X5", "X1", "-->",
                         "X5", "X7", "-->",
                         "X7", "X1", "-->",
                         "X7", "X3", "-->",
                         "X1", "X2", "-->",
                         "X1", "X3", "-->",
                         "X3", "X2", "-->",
                         "X2", "X6", "-->"
                         ), byrow = T, ncol = 3))
expected.sort <- c("X4", "X5", "X7", "X1", "X3", "X2", "X6")

expect_equal(expected.sort, sort(dag))

})

test_that("sort throws a warning and returns NULL when the graph is cyclic", {
  graph <- cgraph(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), edges =
                  matrix(c("X4", "X1", "-->",
                           "X1", "X2", "-->",
                           "X2", "X3", "-->",
                           "X3", "X4", "-->",
                           "X5", "X2", "-->",
                           "X5", "X1", "-->"
                  ), byrow = T, ncol = 3))

  expect_warning(expect_equal(NULL, sort(graph)))
})

test_that("sort decreasing = T works properly", {
  # this is the same dag from the first test
  dag <- cgraph(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), edges =
                  matrix(c("X4", "X5", "-->",
                           "X4", "X7", "-->",
                           "X4", "X1", "-->",
                           "X4", "X2", "-->",
                           "X4", "X6", "-->",
                           "X5", "X1", "-->",
                           "X5", "X7", "-->",
                           "X7", "X1", "-->",
                           "X7", "X3", "-->",
                           "X1", "X2", "-->",
                           "X1", "X3", "-->",
                           "X3", "X2", "-->",
                           "X2", "X6", "-->"
                  ), byrow = T, ncol = 3))
  expected.sort.dec <- c("X6", "X2", "X3", "X1", "X7", "X5", "X4")

  expect_equal(expected.sort.dec, sort(dag, decreasing = T))
})
