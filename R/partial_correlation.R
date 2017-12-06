correlation <- function(x,y) {
  if(length(x) != length(y))
    stop("x and y are different lengths!")
  .Call("c_pearson_correlation", x, y)
}

partial_correlation <- function(x, y, z) {
  if(length(x) != length(y) || length(x) != length(z))
    stop("x and y are different lengths!")
  .Call("c_partial_correlation", x, y, z)
}


