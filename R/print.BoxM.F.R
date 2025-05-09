#' Prints Box'M test based on an F-statistic
#'
#' Prints the results produced by BoxM.F function, with the option to display
#' the matrices involved in the calculations
#'
#' @param x an object of class BoxM.F
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default). The long output shows the
#' covariance matrix for each group and the pooled covariance matrix.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' Displays the results of Box's _M_ test for homogeneity of covariance
#' matrices, based on the F-approximation computed by the \code{BoxM.F}
#' function. The argument `x`, invisibly, as for all print methods, is a list of
#' class "\code{BoxM.F}". This `print` method provides two sorts of output
#' depending on whether the `long` argument is `TRUE` or `FALSE` (the default).
#' The "short" output displays:
#' \itemize{
#'   \item A heading describing the analysis.
#'   \item The data frame analyzed.
#'   \item The variables used for the test.
#'   \item The factor defining the populations or samples and their levels.
#'   \item The value of the Box's \emph{M} statistic, the corresponding
#'   approximate _F_-statistic, the degrees of freedom for the numerator and the
#'   denominator of the _F_-statistic, and the \emph{p}-value.
#' }
#'
#' In addition to the above information, the "long" output lists:
#' \itemize{
#'    \item The covariance matrix for each sample.
#'    \item The pooled covariance matrix.
#' }
#'
#' @examples
#' data(skulls)
#' resBoxM.F <- BoxM.F(skulls, Period)
#' # Long output
#' print(resBoxM.F, long = TRUE)
#'
#' @exportS3Method print BoxM.F
print.BoxM.F <- function(x, long = FALSE, ...) {
  stopifnot(inherits(x, "BoxM.F"))
  cat(" Box's M-test for Homogeneity of Covariance Matrices (F approximation)")
  cat("\n\n Data:", x$data.name, "\n")
  cat(" Variables:", x$variables, "\n")
  cat(" Factor:", x$group, "\n")
  cat(" Levels:", x$levels.group, "\n")
  if (long == TRUE) {
    cat("\nCovariance matrix for each group\n")
    fac <- x$data[, names(x$data) %in% c(x$group)]
    ind <- unique(fac)
    for (i in 1:length(ind)) {
      cat(names(x$Cov.Mat)[[ind[i]]], "\n")
      print(x$Cov.Mat[[ind[i]]])
      cat("\n")
    }
    cat("Pooled Covariance Matrix\n")
    print(x$Cov.pooled)
  }
  cat("\n Box's M =", ifelse(x$BoxM.stat < 0.0001,
                             formatC(x$BoxM.stat, digits = 4, format = "e"),
                             formatC(x$BoxM.stat, digits = 4, format = "f")),
      "\n")
  cat(" F statistic =", formatC(x$F.BoxM, digits = 4, format = "f"),
      ", Num df =", formatC(x$df.v1, digits = 1, format = "f"),
      ", Den df =", formatC(x$df.v2, digits = 1, format = "f"),
      ", p-value =", formatC(x$Pvalue, digits = 4, format = "f"))
  invisible(x)
}
