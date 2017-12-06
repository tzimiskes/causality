# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


cfoo <- function(a,b) {
  .C('cfoo',
     as.double(a),
     as.double(b),
     c = as.double(0))$c }

csq <- function(n, x) {
  out <- .C('sq',
     as.integer(n),
     as.double(x))
  return(out)
}

correlation <- function(x,y) {
.C("c_correlation", as.double(x), as.double(y), as.integer(length(x)), rho = as.double(0))$rho
}
