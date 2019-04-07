# these hidden variables are used to assign (sub)classes to craph objects
.CGRAPH_CLASS  <- c(                     "causality.graph")
.DAG_CLASS     <- c("causality.dag"    , "causality.graph")
.PDAG_CLASS    <- c("causality.pdag"   , "causality.graph")
.PATTERN_CLASS <- c("causality.pattern", "causality.graph")
.PAG_CLASS     <- c("causality.pag"    , "causality.graph")

# Edge types currently used in causality graphs
.DIRECTED       <- "-->"
.UNDIRECTED     <- "---"
.PLUSPLUS       <- "++>"
.SQUIGGLE       <- "~~>"
.CIRCLEDIRECTED <- "o->"
.CIRCLECIRCLE   <- "o-o"
.BIDIRECTED     <- "<->"

# edges that show up in PDAGs: -->, ---
.NONLATENT_EDGE_TYPES <- c(.DIRECTED, .UNDIRECTED)

# edges that show up in PAGs ~~>, ++>, o->, o-o, <->
.LATENT_EDGE_TYPES <- c(.SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED, .CIRCLECIRCLE,
                                   .BIDIRECTED)

.EDGE_TYPES <- c(.LATENT_EDGE_TYPES, .NONLATENT_EDGE_TYPES)

# edges of the type -->, ~~>, ++>, o->
.DIRECTED_EDGE_TYPES <- c(.DIRECTED, .SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED)
