library(causality)
context("Test causality IO")

test_that("read_causality_graph works", {
  graph <- read_causality_graph("../read.test")
  expect_equal(shd(graph, sachs.dag), 0)

})


test_that("write_causality_graph works", {
  skip_on_cran()
  write_causality_graph("/tmp/write.test", sachs.dag)
  graph <- read_causality_graph("/tmp/write.test")
  expect_equal(shd(graph, sachs.dag), 0)
})
