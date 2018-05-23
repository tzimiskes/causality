.DIRECTED       <- "-->"
.UNDIRECTED     <- "---"
.PLUSPLUS       <- "++>"
.SQUIGGLE       <- "~~>"
.CIRCLEDIRECTED <- "o->"
.CIRCLECIRCLE   <- "o-o"
.BIDIRECTED     <- "<->"

# edges that show up in pags
.LATENT_EDGE_TYPES    <- c(.DIRECTED, .SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED,
                           .CIRCLECIRCLE)
#edges that show up in pdags
.NONLATENT_EDGE_TYPES <- c(.DIRECTED, .UNDIRECTED)

