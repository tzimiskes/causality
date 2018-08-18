library(causality)

context("meek rules work")

test_that("meek rule 1 works", {
  meek1.test <- pdag(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"),
                edges = matrix(c("X1", "X2", "-->",
                                 "X2", "X3", "---",
                                 "X4", "X5", "-->",
                                 "X6", "X5", "---",
                                 "X7", "X8", "-->",
                                 "X8", "X9", "---",
                                 "X7", "X9", "---"), ncol = 3, byrow = T)
)


  meek1.test.ans <-
    pdag(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"),
         edges = matrix(c("X1", "X2", "-->",
                          "X2", "X3", "-->",
                          "X4", "X5", "-->",
                          "X5", "X6", "-->",
                          "X7", "X8", "-->",
                          "X8", "X9", "---",
                          "X7", "X9", "---"), nrow = 7 , ncol = 3, byrow = T)
  )

expect_equal(arrowhead_precision(meek(meek1.test),  meek1.test.ans) == 1,
             arrowhead_recall(meek(meek1.test), meek1.test.ans) == 1)
})

test_that("meek rule 2 works", {

  meek2.test <-
    pdag(nodes = c("X1", "X2", "X3", "X4", "X5", "X6"), edges = matrix(
      c("X1", "X2", "-->",
        "X2", "X3", "-->",
        "X3", "X1", "---",
        "X4", "X5", "-->",
        "X4", "X6", "---",
        "X5", "X6", "-->"), ncol = 3, byrow = T
    )
  )

  meek2.test.ans <-
    pdag(nodes = c("X1", "X2", "X3", "X4", "X5", "X6"),
         edges = matrix(c("X1", "X2", "-->",
                          "X2", "X3", "-->",
                          "X1", "X3", "-->",
                          "X4", "X5", "-->",
                          "X4", "X6", "-->",
                          "X5", "X6", "-->"), ncol = 3, byrow = T)
  )
  # arrowhead_precision and arrowhead_recall between the two should be one
  expect_equal(arrowhead_precision(meek(meek2.test), meek2.test.ans) == 1,
               arrowhead_recall(meek(meek2.test), meek2.test.ans) == 1)
})

test_that("meek rule 3 works",{

meek3.test <-
  pdag(nodes = paste0("X", 1:12),
       edges = matrix(c("X10", "X9" , "---",
                        "X11", "X10", "-->",
                        "X12", "X10", "-->",
                        "X2" , "X1" , "---",
                        "X3" , "X1" , "---",
                        "X3" , "X2" , "-->",
                        "X4" , "X1" , "---",
                        "X4" , "X2" , "-->",
                        "X6" , "X5" , "---",
                        "X6" , "X7" , "---",
                        "X6" , "X8" , "---",
                        "X7" , "X5" , "-->",
                        "X8" , "X5" , "-->",
                        "X11", "X9" , "---",
                        "X12", "X9" , "---"), ncol = 3, byrow = T)

)
  meek3.test.ans <-
    pdag(nodes = paste0("X", 1:12),
         edges = matrix(c("X9" , "X10" , "-->",
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
                          "X12", "X9"  , "---"), ncol = 3, byrow = T)
  )
  expect_equal(arrowhead_precision(meek(meek3.test), meek3.test.ans) == 1,
               arrowhead_recall(meek(meek3.test), meek3.test.ans) == 1)
})

test_that("meek rule 4 works",{

  meek4.test <-
    pdag(nodes = paste0("X", 1:20),
         edges = matrix(c("X1" , "X4" , "---",
                          "X10", "X11", "---",
                          "X10", "X12", "---",
                          "X9" , "X10", "---",
                          "X11", "X9" , "-->",
                          "X12", "X11", "-->",
                          "X14", "X13", "---",
                          "X15", "X13", "---",
                          "X15", "X14", "-->",
                          "X16", "X13", "---",
                          "X16", "X15", "-->",
                          "X18", "X17", "---",
                          "X19", "X17", "---",
                          "X19", "X20", "-->",
                          "X1" , "X2" , "---",
                          "X20", "X17", "---",
                          "X20", "X18", "-->",
                          "X3" , "X1" , "-->",
                          "X3" , "X2" , "-->",
                          "X4" , "X3" , "-->",
                          "X5" , "X8" , "-->",
                          "X5" , "X6" , "---",
                          "X7" , "X5" , "---",
                          "X7" , "X8" , "-->",
                          "X8" , "X6" , "-->"), ncol = 3, byrow = T)

    )
  meek4.test.ans <-
    pdag(nodes = paste0("X", 1:20),
         edges = matrix(c("X4"  , "X1"  , "-->",
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
                          "X8"  , "X6"  , "-->"), ncol = 3, byrow = T)
    )
  expect_equal(arrowhead_precision(meek(meek4.test), meek4.test.ans) == 1,
               arrowhead_recall(meek(meek4.test), meek4.test.ans) == 1)
})