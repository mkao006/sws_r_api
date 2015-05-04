kdeMode <- function(x, ...) {
  d <- density(x, ...)
  d$x[d$y == max(d$y]
}