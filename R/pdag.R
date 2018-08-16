
#' @export
is.pdag <-function(cgraph) {
  if (isTRUE(all.equal(.PDAG_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

as.pdag <- function(cgraph) {
  if(!is.cgraph(cgraph))
    stop("Input is not a causality graph")
  if(is.pdag(cgraph))
    return(cgraph)

  if (is.nonlatent(cgraph)) {
    if (!is.cyclic(cgraph)) {
      class(cgraph) <- .PDAG_CLASS
      return(cgraph)
    }
  }
  else
    stop("input contains a cycle, so it cannot be coerced to pdag")
}

as.pdag.causality.dag <- function(cgraph) {
  if(!is.dag(cgraph))
    stop("Input is not a causality dag")
  class(cgraph) = .PDAG_CLASS
  return(cgraph)
}

as.pdag.causality.pattern <- function(cgraph) {
  if(!is.pattern(cgraph))
    stop("Input is not a causality pattern")
  class(cgraph) = .PDAG_CLASS
  return(cgraph)
}

as.pdag.causality.pag <- function(cgraph) {
  if(!is.pag(cgraph))
    stop("Input is not a causality pag")

  stop("not implemented")
}