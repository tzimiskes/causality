correlation <- function(x,y) {
  if (length(x) != length(y))
    stop("x and y are different lengths!")
  if(any(is.na(x)))
    stop("x contains NAs")
  if(any(is.na(y)))
    stop("y contains NAs")
  # parse input to enusre it is of the correct type
  if (!is.double(x))
    x <- coerce_to_double(x)
  if (!is.double(y))
    y <- coerce_to_double(y)

  .Call("c_pearson_correlation", x, y)
}

partial_correlation <- function(x, y, z) {
  if (length(x) != length(y) || length(x) != length(z))
    stop("x,y and z do not all have the same length!")
  # check to see if the vectors contain NAs
  if(any(is.na(x)))
    stop("x contains NAs")
  if(any(is.na(y)))
    stop("y contains NAs")
  if(any(is.na(z)))
    stop("z contains NAs")
  # parse input to enusre it is of the correct type
  if (!is.double(x))
    x <- coerce_to_double(x)
  if (!is.double(y))
    y <- coerce_to_double(y)
  if (!is.double(z))
    z <- coerce_to_double(z)
  # input seems ok, so lets go
  .Call("c_partial_correlation", x, y, z)
}

coerce_to_double <- function(x) {
  if (is.integer(x)) {
    message("coercing x (of type integer) to numeric")
    return(as.double(x))
  }
  if (is.ordered(x)) {
    message("coercing x (of type ordered factor) to numeric")
    return(as.double(x))
  }
  return(stop("cannot coerce x to numeric! is it an unordered factor?"))
}