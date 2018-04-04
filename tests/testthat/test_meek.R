library(causality)

context("meek rules work")

load("../meek12tests")

test_that("meek rule 1 works", {
  load("../meek1_test")

  meek1_answers <- matrix(
  c("X1", "X2", "-->",
    "X2", "X3", "-->",
    "X4", "X5", "-->",
    "X5", "X6", "-->",
    "X7", "X8", "-->",
    "X8", "X9", "---",
    "X7", "X9", "---"), nrow = 7 , ncol = 3, byrow = T
  )

expect_equal(meek(meek1_test)$edges, meek1_answers)

})

test_that("meek rule 2 works", {
  load("../meek2_test")
  meek2_answers <- matrix(
    c("X1", "X2", "-->",
      "X2", "X3", "-->",
      "X1", "X3", "-->",
      "X4", "X5", "-->",
      "X4", "X6", "-->",
      "X5", "X6", "-->"), nrow = 6, ncol = 3, byrow = T
  )
  expect_equal(meek(meek2_test)$edges, meek2_answers)
})

meek2_answers <- matrix(
c("X1", "X2", "-->",
  "X2", "X3", "-->",
  "X1", "X3", "---",
  "X4", "X5", "-->",
  "X5", "X6", "-->",
  "X6", "X5", "---"), nrow = 6, ncol = 3, byrow = T
)
meek_rule2_test
