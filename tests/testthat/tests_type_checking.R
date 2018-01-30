library(causality)

context("type checking works")

#technically, the function import_from_tetrad_file tests is_valid_cgraph by attempting to
#import multigraphs etc it does that by building a "cgraph" object without
#setting its class to "cgraph", and then uses is_valid_cgraph to check whether
#or not the proposed cgraph is a legal cgraph object
#test_that("is_valid_graph works",
#)

test_that("as.dag accepts dags and rejects non-dags", {
  expect_s3_class(import_from_tetrad_file("../dag_test.txt", "dag"), "dag")
  expect_error(as.dag(import_from_tetrad_file("../dcg_test.txt", type = "dag")))
})