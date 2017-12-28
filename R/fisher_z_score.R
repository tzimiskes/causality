fisher_z_score <- function(r_hat, n) {
  if (is.na(r_hat))
    stop("r_hat is NA")
  if (is.na(n))
    stop("n is NA")
  if (abs(r_hat) > 1)
    stop("r_hat is not in [-1,1]")
  if (n < 0)
    stop("n < 0")

  # parse input to ensure it is of the correct type
  if (!is.double(r_hat))
    stop("r_hat is not of type double")
  if (!is.integer(n))
    n <- as.integer(n)

  .Call("c_fisher_transformation_z_score", r_hat, n)
}

#' Test the Independence Between Continous variables.
#'
#' \code{test_fisher_independence} returns whether or not the \code{z_score} implies independence at the given signifigence level \code{alpha}
#' @param z_score Fisher z-score from (ideally) \code{fisher_z_score}, or elsewhere
#' @param alpha The signifigence level of the test, i.e., .05, .01, .001 etc
#' @return \code{TRUE} if idependent or \code{FALSE} if not.
#' @details
#' The test is H0: x, y are independent and H1: x,y are dependent.
#' @examples
#' test_fisher_independence(5, .05)
#' test_fisher_independence(100,.001)
#' @seealso fisher_z_score
test_fisher_independence <- function(z_score, alpha) {
  # check input type
  if (!is.double(z_score))
    stop("r_hat is not of type double")
  if (!is.double(alpha))
    stop("alpha is not a double")

  # check to see if input is there and if alpha is in range
  if (is.na(z_score))
    stop("r_hat is NA")
  if (is.na(alpha))
    stop("alpha is NA")
  if (alpha <= 0 || alpha >= 1)
    stop("alpha is not in (0, 1")


  .Call("c_is_independent", z_score, alpha)
}