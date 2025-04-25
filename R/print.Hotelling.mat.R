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
#' @returns
#' Displays the results of the comparison of mean values of two multivariate
#' samples, under the assumption that covariance matrices are equal, using
#' Hotelling's T² test. The argument `x`, invisibly, as for all print methods,
#' is a list of class "\code{Hotelling.mat}". This `print` method provides two
#' sorts of output depending on whether the `long` argument is `TRUE` or `FALSE`
#' (the default). The "short" output displays:
#' \itemize{
#'   \item A description of the analysis.
#'   \item The data frame analyzed.
#'   \item The labels of the two-level group factor (samples), with an order
#'   determined by the user in the `Hotelling.mat` argument `level1`.
#'   \item The value of Hotelling's \eqn{T²}-statistic.
#'   \item The value of the \emph{F}-statistic with its corresponding degrees of
#'       freedom for numerator and denominator.
#'   \item The P-value.
#'  }
#'
#' In addition to this summary, the "long" output shows:
#'
#'  \itemize{
#'   \item The mean vectors and covariance matrices for each sample.
#'   \item The pooled covariance matrix.
#'   \item The inverse of the covariance matrix.
#'  }
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
  cat(" (Assuming equal covariance matrices)\n")
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
