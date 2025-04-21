#' Prints Hotelling's \eqn{T^2} test
#'
#' Prints the results produced by the \code{\link[smsets]{Hotelling.mat}}
#' function
#'
#' @param x an object of class \code{"Hotelling.mat"}
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' data(sparrows)
#' results.T2 <- Hotelling.mat(sparrows, group = Survivorship, level1 = "S")
#' # Long output
#' print(results.T2, long = TRUE)
#'
#' @exportS3Method print Hotelling.mat
print.Hotelling.mat <- function(x, long = FALSE, ...) {
  stopifnot(inherits(x, "Hotelling.mat"))
  cat(" Hotelling's T2 test for the comparison of two multivariate samples\n")
  #
  cat(" Data:  ", x$data.name, "\n")
  cat(" Group levels: (1)", x$levels.group[1], ";  (2)", x$levels.group[2],
      "\n\n")
  if (long == TRUE) {
    cat("Mean vectors and Covariance Matrices\n\n")
    for (i in 1:6) {
      if (i == 2 | i == 4) { cat("Covariance Matrix:\n")}
      if (i == 5) { cat("Pooled Covariance Matrix:\n")}
      if (i == 6) { cat("Inverse of Covariance Matrix:\n")}
      print(x$T2.list[[i]])
      cat("\n")
    }
  }
  cat("Hotelling's T2 statistic =",
      formatC(x$T2.list[[7]], digits = 4, format = "f"), "\n")
  cat("F statistic =",
      formatC(x$T2.list[[8]], digits = 4, format = "f"), "\n")
  cat("Numerator df =",
      formatC(x$T2.list[[9]][1], digits = 2, format = "d"), "\n")
  cat("Denominator df =",
      formatC(x$T2.list[[9]][2], digits = 2, format = "d"), "\n")
  cat("P-value =",
      formatC(x$T2.list[[10]], digits = 4, format = "f"), "\n")
  invisible(x)
}
