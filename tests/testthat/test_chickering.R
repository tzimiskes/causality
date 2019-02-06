library(causality)

context("Chickering works")

# This test checks to see that chickering correctly identifies Y
# structures as a pattern
test_that("Chickering generates the correct pattern (simple)", {
dag <- dag(nodes = c("X1", "X2", "X3", "X4"), edges =
           matrix(c("X1", "X3", "-->",
                    "X2", "X3", "-->",
                    "X3", "X4", "-->"), byrow = T, ncol = 3))

pattern <- pattern(nodes = c("X1", "X2", "X3", "X4"), edges =
                   matrix(c("X1", "X3", "-->",
                            "X2", "X3", "-->",
                            "X3", "X4", "-->"), byrow = T, ncol = 3),
                   validate = F)

expect_equal(shd(chickering(dag), pattern), 0)
})

# This more complicated test checks to see that chickering correctly generates
# the pattern
test_that("Chickering works", {
  dag <- dag(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), edges =
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

pattern <- pattern(nodes = c("X1", "X2", "X3", "X4", "X5", "X6", "X7"), edges =
                  matrix(c("X4", "X5", "---",
                           "X4", "X7", "---",
                           "X4", "X1", "---",
                           "X4", "X2", "-->",
                           "X4", "X6", "-->",
                           "X5", "X1", "---",
                           "X5", "X7", "---",
                           "X7", "X1", "---",
                           "X7", "X3", "---",
                           "X1", "X2", "-->",
                           "X1", "X3", "---",
                           "X3", "X2", "-->",
                           "X2", "X6", "-->"
                  ), byrow = T, ncol = 3), validate = F)

expect_equal(shd(chickering(dag), pattern), 0)
})

# a chordal graph has no V structures, so its pattern is completely undirected
# sachs.dag is a graph that is included in the causality package
test_that("chordal DAGs become undirected", {

sachs.pattern <-
  pattern(nodes <- sachs.dag$nodes,
          edges = matrix(c("Raf",  "Mek",  "---",
                           "Plcg", "PIP3", "---",
                           "Plcg", "PIP2", "---",
                           "PKC",  "Raf",  "---",
                           "PKC",  "PKA",  "---",
                           "PKC",  "P38",  "---",
                           "PKC",  "Mek",  "---",
                           "PKC",  "Jnk",  "---",
                           "PKA",  "Raf",  "---",
                           "PKA",  "P38",  "---",
                           "PKA",  "Mek",  "---",
                           "PKA",  "Jnk",  "---",
                           "PKA",  "Erk",  "---",
                           "PKA",  "Akt",  "---",
                           "PIP3", "PIP2", "---",
                           "Mek",  "Erk",  "---",
                           "Erk",  "Akt",  "---"), ncol = 3, byrow = T),
          validate = F)

  expect_equal(shd(sachs.pattern, chickering(sachs.dag)), 0)
})
