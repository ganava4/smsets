#' Prints a one-way MANOVA with extra information
#'
#' Prints the results produced by the \code{\link[smsets]{OnewayMANOVA}}
#' function
#'
#' @param x An object of class \code{OnewayMANOVA}.
#' @param test The name of the test statistic to be used (the four tests
#' implemented in \code{summary.manova}). Pillai's test is the default. Partial
#' matching is used so the name can be abbreviated.
#' @param long A logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' data(skulls)
#' res.MANOVA <- OnewayMANOVA(skulls, group = Period)
#' # Long output, Wilks' test
#' print(res.MANOVA, test = "Wilks", long = TRUE)
#'
#' @exportS3Method print OnewayMANOVA
print.OnewayMANOVA <- function(x,
                               test = c("Pillai", "Wilks", "Hotelling-Lawley",
                                        "Roy"), long = FALSE, ...)
{
  stopifnot(inherits(x, "OnewayMANOVA"))
  cat(" One-factor Multivariate Analysis of Variance with extra matrix info\n")
  #
  cat("\n Data:", x$data.name, "\n")
  cat(" Variables:", x$variables, "\n")
  cat(" Factor:", x$group, "\n")
  cat(" Levels:", x$x.mnv$xlevels[[1]], "\n")
  if (long == TRUE) {
    cat("\nBetween-Sample Matrix of Sum of Squares and Crossed Products, B\n")
    print(x$B)
    cat("\nWithin Sample Matrix of Total Sum of Squares and Crossed Products, W\n")
    print(x$W)
    cat("\nTotal Sample Matrix of Sum of Squares and Crossed Products, T\n")
    print(x$T)
  }
  cat("\n                      One-Way MANOVA\n")
  summ.MANOVA <- summary(x$x.mnv, test = test)
  print(summ.MANOVA)
  invisible(x)
}
