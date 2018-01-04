shd(true_pattern, est_pattern) {
  if (!("pattern" %in% class(true_pattern))) {
    if ("dag" %in% class(true_pattern))
      stop("true_pattern is of class dag, not pattern. Convert it to pattern via dag_to_pattern first")
    else
      stop("true_pattern is not a pattern")
  }
  if (!("pattern" %in% class(est_pattern))) {
    if ("dag" %in% class(est_pattern))
      stop("est_pattern is of class dag, not pattern. Convert it to pattern via dag_to_pattern first")
    else
      stop("est_pattern is not a pattern")
  }
  distance <- 0

  return(distance)
}