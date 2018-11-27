#' @useDynLib causality causalityFruchtermanReingold
#' @export
export_tikz <- function(cgraph, file = stdout(), iterations = 10, height = 3,
                                width = 3)
{


  positions <- .Call("causalityFruchtermanReingold", graph = cgraph,
                                                     nIterations = iterations,
                                                     height = height,
                                                     width = width)
  print(positions)
  edge_style <- function(edge) {
    switch (edge,
      "-->" = "directed",
      "---" = "undirected",
      "o->" = "circ-directed, uncertain",
      "o-o" = "circ-circ, uncertain",
      "++>" = "directed, certain",
      "~~>" = "directed",
      "<->" = "bidirected"
    )
  }
  preamble <- c("\\documentclass[border = 3px]{standalone}\n",
                "\\usepackage{tikz}\n",
                "\\usetikzlibrary{shapes, arrows.meta}\n")
  cat(preamble, file = file, sep = "")

  cat("\\begin{document}\n", file = file, append = T)
  cat("\\begin{tikzpicture}[{shorten >=1pt, >=stealth, scale = 1.5}]\n", file = file, append = T)

  tikz.defs <- c("\\tikzstyle{node}          = [draw, fill = white, circle, scale =.75, line width = .7]\n",
                 "\\tikzstyle{directed}      = [-{Latex[length = 2.5mm]}]\n",
                 "\\tikzstyle{circ-directed} = [{Circle[open, length=2.5mm]}-{Latex[length = 3mm]}]\n",
                 "\\tikzstyle{circ-circ}     = [{Circle[open, length = 2.5mm]}-{Circle[open, length = 2.5mm]}]\n",
                 "\\tikzstyle{circ-circ}     = [{Circle[open, length = 2.5mm]}-{Circle[open, length = 2.5mm]}]\n",
                 "\\tikzstyle{bidirected}    = [{Latex[length=3mm]}-{Latex[length=3mm]}]\n",
                 "\\tikzstyle{certain}       = [green!85!black]\n",
                 "\\tikzstyle{uncertain}     = [dashed, black]\n")

  cat(tikz.defs, file = file, sep = "", append = T)
  nodes <- cgraph$nodes
  for (node in nodes) {
      x <- positions[match(node, nodes), 1]
      y <- positions[match(node, nodes), 2]
      if (!is.null(cgraph$adjacencies[[node]]))
        cat(sprintf("\\node[node] at (%f, %f) (%s) {%s};\n", x, y, node, node),
                      file = file, append = T)

  }

  cat("\n", file = file, append = T)
  edges <- cgraph$edges
  for (i in 1:nrow(edges)) {
      x    <- edges[i, 1]
      y    <- edges[i, 2]
      edge.type <- edge_style(edges[i, 3])
      s <- sprintf("\\draw[%s, line width = 1] (%s) -- (%s);\n", edge.type, x, y)
      cat(s, file = file, append = T)
  }
  cat("\\end{tikzpicture}\n", file = file, append = T)
  cat("\\end{document}\n", file = file, append = T)

}
