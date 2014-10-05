#' relative_error
#'
#' Calculate the relative error between two variables, x and y.
#'
#' @param x the demoninator
#' @param y the first value of the difference
#'
#' @section Equation \eqn{()y - x) / x}

relative_error <- function(x, y) {
  diff <- y - x
  re <- diff / x
  return(re)
}