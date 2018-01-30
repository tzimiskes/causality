library(causality)
context("importing tetrad files")

test_that("invalid graphs are rejected", {
  expect_error(import_from_tetrad_file("../multi_graph.txt"))
  # expect warning from "reversing" <--
  # error itself is because the graph is a multigraph
  expect_error(expect_warning(import_from_tetrad_file("../reversed_multigraph.txt")))
  expect_error(import_from_tetrad_file("../self_loop.txt"))
  expect_s3_class(import_from_tetrad_file("../dag_test.txt", type = "dag"), "dag")
  expect_error(import_from_tetrad_file("../dcg_test.txt", type = "dag"))
  expect_s3_class(import_from_tetrad_file("../dcg_test.txt", type = NULL), "cgraph")
})
