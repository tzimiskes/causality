shd <- function(true_pattern, est_pattern) {
  if (!any(c("pattern", "pdag") %in% class(true_pattern))) {
    if ("dag" %in% class(true_pattern))
      stop("true_pattern is of class dag, not pattern. Convert it to pattern via dag_to_pattern first")
    else
      stop("true_pattern is not a pattern or pdag")
  }
  if (!any(c("pattern", "pdag") %in% class(est_pattern))) {
    if ("dag" %in% class(est_pattern))
      stop("est_pattern is of class dag, not pattern. Convert it to pattern via dag_to_pattern first")
    else
      stop("est_pattern is not a pattern or pdag")
  }
  true_children <- list()
  for (i in 1:nrow(true_pattern$edges)) {
    true_edge <- true_pattern$edges[i,]
    true_children[[true_edge[1]]][[true_edge[2]]] <- true_edge[3]
  }
  est_children <- list()
  for (j in 1:nrow(est_pattern$edges)) {
    est_edge <- est_pattern$edges[j,]
    est_children[[est_edge[1]]][[est_edge[2]]] <- est_edge[3]
  }
  distance <- 0
  for (i in 1:nrow(true_pattern$edges)) {
    true_edge <- true_pattern$edges[i, ]
    est_edge <- as.list(est_children[[true_edge[1]]])[[true_edge[2]]]
    if (is.null(est_edge)) {
      if (true_edge[3] == "-->") {
        print(true_edge)
        distance <- distance + 1
      }
      # if true_edge is not of type -->, it is ---
      # since (true_src, true_dst, ---) is not in true graph, we ned to check to see if (true_dst, true_src, ---) is
      else {
        est_edge <- as.list(est_children[[true_edge[2]]])[[true_edge[1]]]
        # nope
        if (is.null(est_edge) || est_edge != "---") {
          print(true_edge)
          distance <- distance + 1
        }
      }
      # est_edge is not null
      # if the orientations don't match, est_edge in not oriented in
    } else if( est_edge != true_edge[3]) {
      print(true_edge)
      distance <- distance + 1
    }
  }
  # we only need to see if est_edge is extra in the true pdagsj
  for(i in 1:nrow(est_pattern$edges)) {
    est_edge <- est_pattern$edges[i, ]
    true_edge <- as.list(true_children[[est_edge[1]]])[[est_edge[2]]]
    if (is.null(true_edge)) {
      true_edge <- as.list(true_children[[est_edge[2]]])[[est_edge[1]]]
      if (is.null(true_edge)) {
        print(est_edge)
        distance <- distance + 1
      }
    }
  }
  return(distance)
}

