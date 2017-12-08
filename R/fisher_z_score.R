fisher_z_score <- function(r_hat, n) {
  if (is.na(r_hat))
    stop("r_hat is NA")
  if (is.na(n))
    stop("n is NA")
  if (abs(r_hat) > 1)
    stop("r_hat is not in [-1,1]")
  if (n < 0)
    stop("n < 0")

  # parse input to enusre it is of the correct type
  if (!is.double(r_hat))
    stop("r_hat is not of type double")
  if (!is.integer(n))
    n <- as.integer(n)

  .Call("c_fisher_transformation_z_score", r_hat, n)
}


is_independent <- function(z_score, alpha) {
  if (is.na(z_score))
    stop("r_hat is NA")
  if (is.na(alpha))
    stop("alpha is NA")
  if (alpha <= 0 || alpha > 1)
    stop("alpha is not in (0,1]")

  # check input type
  if (!is.double(z_score))
    stop("r_hat is not of type double")
  if (!is.double(alpha))
    stop("alpha is not a double")

  .Call("c_is_independent", z_score, alpha)
}