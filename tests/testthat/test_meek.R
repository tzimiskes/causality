library(causality)

context("meek rules work")


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

test_that("meek rule 3 works",{

load("../meek3_test")
  meek3_answers <- matrix(
    c("X9" , "X10" , "-->",
      "X11", "X10" , "-->",
      "X12", "X10" , "-->",
      "X1" , "X2"  , "-->",
      "X3" , "X1"  , "---",
      "X3" , "X2"  , "-->",
      "X4" , "X1"  , "---",
      "X4" , "X2"  , "-->",
      "X6" , "X5"  , "-->",
      "X6" , "X7"  , "---",
      "X6" , "X8"  , "---",
      "X7" , "X5"  , "-->",
      "X8" , "X5"  , "-->",
      "X11", "X9"  , "---",
      "X12", "X9"  , "---"), nrow = 15, ncol = 3, byrow = T
  )
  expect_equal(meek(meek3_test)$edges, meek3_answers)
})

test_that("meek rule 4 works",{

  load("../meek4_test")
  meek4_answers <- matrix(
    c("X4"  , "X1"  , "-->",
      "X10" , "X11" , "---",
      "X10" , "X12" , "---",
      "X10" , "X9"  , "-->",
      "X11" , "X9"  , "-->",
      "X12" , "X11" , "-->",
      "X13" , "X14" , "-->",
      "X15" , "X13" , "---",
      "X15" , "X14" , "-->",
      "X16" , "X13" , "---",
      "X16" , "X15" , "-->",
      "X17" , "X18" , "-->",
      "X19" , "X17" , "---",
      "X19" , "X20" , "-->",
      "X1"  , "X2"  , "-->",
      "X20" , "X17" , "---",
      "X20" , "X18" , "-->",
      "X3"  , "X1"  , "-->",
      "X3"  , "X2"  , "-->",
      "X4"  , "X3"  , "-->",
      "X5"  , "X8"  , "-->",
      "X5"  , "X6"  , "-->",
      "X7"  , "X5"  , "---",
      "X7"  , "X8"  , "-->",
      "X8"  , "X6"  , "-->"), nrow = 25, ncol = 3, byrow = T
  )
  expect_equal(meek(meek4_test)$edges, meek4_answers)
})