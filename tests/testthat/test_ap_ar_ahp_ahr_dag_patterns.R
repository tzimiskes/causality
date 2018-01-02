context("AP, AR, AHP, AHR works on DAGs and patterns")
# this file probablly needs a better name

test_that("Comparing DAG-Patterns works", {
  load("../../save/patterns")

  expect_equal(adjacency_precision(true_pattern, est_pattern) , 10/10)
  expect_equal(adjacency_recall(true_pattern, est_pattern), 9/10)

  # the test graphs contain bidirected edges, so both function should throw
  # warnings, and compute the following numbers
  expect_warning(
  expect_equal(arrowhead_precision(true_pattern, est_pattern), 6/9)
  )
  expect_warning(
  expect_equal(arrowhead_recall(true_pattern, est_pattern), 4/10)
  )
})

test_that("Comparing DAGs works", {
  load("../../save/dags")

  expect_equal(adjacency_precision(true_dag, est_dag) , 10/10)
  expect_equal(adjacency_recall(true_dag, est_dag), 9/10)
  expect_equal(arrowhead_precision(true_dag, est_dag), 5/9)
  expect_equal(arrowhead_recall(true_dag, est_dag), 5/10)
})