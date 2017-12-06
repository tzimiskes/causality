cor_ar <- function(x, y) {
  mu_x <- mean(x)
  mu_y <- mean(y)
  x <- x - mu_x
  y <- y - mu_y
  num <- sum(x * y)
  sd_x <- sqrt(sum(x^2))
  sd_y <- sqrt(sum(y^2))

  num/(sd_x*sd_y)
}
