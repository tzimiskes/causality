library(causality)

context("PDX works")

# This test checks to see that chickering correctly identifies Y
# structures as a pattern

test_that("PDX correctly fails to generate an extension", {
  pdag <- pdag(nodes = c("X1", "X2", "X3", "X4"), edges =
                 matrix(c("X1", "X2", "-->",
                          "X2", "X3", "---",
                          "X3", "X4", "-->",
                          "X4", "X1", "-->"), byrow = T, ncol = 3))
  expect_warning(expect_null(pdx(pdag)))
})

# This more complicated test checks to see that chickering correctly generates
# the pattern
test_that("PDX works (simple)", {
  pdag <- pdag(nodes = c("X1", "X2", "X3", "X4"), edges =
                 matrix(c("X1", "X3", "-->",
                          "X2", "X3", "-->",
                          "X3", "X4", "---"), byrow = T, ncol = 3))
  dag <- dag(nodes = c("X1", "X2", "X3", "X4"), edges =
               matrix(c("X1", "X3", "-->",
                        "X2", "X3", "-->",
                        "X3", "X4", "-->"), byrow = T, ncol = 3))

  # currently shd doesn't support a metric on dag space, though this may change
  # in the future (it is't even a trivial extension)
  expect_equal(arrowhead_recall(pdx(pdag), dag) == 1,
               arrowhead_precision(pdx(pdag), dag) == 1)
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

  expect_equal(shd(chickering(pdx(sachs.pattern)), sachs.pattern), 0)
})
