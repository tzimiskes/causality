(* should be fib(n-1) + fib(n-2) *) 
rt_tikz <- function(cgraph) {
  
  match_edge <- function(edge) {
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
  cat(preamble, file = stdout(), sep = "")
  
  cat("\\begin{document}\n", file = stdout(), append = T)
  cat("\\begin{tikzpicture}[{shorten >=1pt, >=stealth, scale = 1.5}]\n", file = stdout(), append = T) 
  
  tikz.defs <- c("\\tikzstyle{node}          = [draw, fill = white, circle, scale =.75, line width = .7]",
                 "\\tikzstyle{directed}      = [-{Latex[length = 2.5mm]}]\n",
                 "\\tikzstyle{circ-directed} = [{Circle[open, length=2.5mm]}-{Latex[length = 3mm]}]\n",
                 "\\tikzstyle{circ-circ}     = [{Circle[open, length = 2.5mm]}-{Circle[open, length = 2.5mm]}]\n",
                 "\\tikzstyle{circ-circ}     = [{Circle[open, length = 2.5mm]}-{Circle[open, length = 2.5mm]}]\n",
                 "\\tikzstyle{bidirected}    = [{Latex[length=3mm]}-{Latex[length=3mm]}]\n",
                 "\\tikzstyle{certain}       = [green!85!black]\n",
                 "\\tikzstyle{uncertain}     = [dashed, black]\n")  

  cat(tikz.defs, file = stdout(), sep = "")
  nodes <- cgraph$nodes
  for (node in nodes)
    cat(sprintf("\\node[node] at (0, 0) (%s) {%s};\n", node, node), 
        file = stdout(), append = T)
  
  cat("\n", file= stdout(), append = T)
  edges <- cgraph$edges 
  for (i in 1:nrow(edges)) {
      x    <- edges[i, 1]
      y    <- edges[i, 2]
      edge.type <- match_edge(edges[i, 3])
      s <- sprintf("\\draw[%s, line width = 1] (%s) -- (%s);\n", edge.type, x, y)
      cat(s, file = stdout(), append = T)
  }
  cat("\\end{tikzpicture}\n", file = stdout(), append = T) 
  cat("\\end{document}\n", file = stdout(), append = T)
  
}


