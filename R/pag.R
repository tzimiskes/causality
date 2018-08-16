#' @export
is.pag <-function(cgraph) {
  if (isTRUE(all.equal(.PAG_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

# Causality Graph as.pag Functions ---------------------------------------------
as.pag <- function(cgraph) {
  stop("not implemented")
}