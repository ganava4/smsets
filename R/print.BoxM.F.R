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
