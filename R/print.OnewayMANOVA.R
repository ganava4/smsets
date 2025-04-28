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
#' @returns
#' Displays the results of a One-way MANOVA, i.e., the test of the difference of
#' mean vectors among the levels of a single factor with respect to _p_ response
#' variables. The argument `x`, invisibly, as for all print methods, is a list
#' of class "\code{OnewayMANOVA}". This `print` method provides two sorts of
#' output depending on whether the `long` argument is `TRUE` or `FALSE` (the
#' default). The "short" output displays:
#' \itemize{
#'   \item A heading describing the function.
#'   \item The data frame analyzed.
#'   \item The variables involved in the calculation of distances.
#'   \item The factor defining the populations or samples and their levels.
#'   \item The One-way MANOVA table specifying the `test` chosen for the
#'   F-test approximation, like in `summary.manova`.
#' }
#'
#' In addition to the above information, the "long" output lists:
#' \itemize{
#'    \item The Between-Sample Sum of Squares and Crossed Products matrix, B
#'    \item The Within-Sample Total Sum of Squares and Crossed Products matrix,
#'    W.
#'    \item The Total Sample Sum of Squares and Crossed Products matrix, T.
#' }
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
    cat("\nBetween-Sample Sum of Squares and Crossed Products Matrix, B\n")
    print(x$B)
    cat("\nWithin-Sample Sum of Squares and Crossed Products Matrix, W\n")
    print(x$W)
    cat("\nTotal Sum of Squares and Crossed Products Matrix, T\n")
    print(x$T)
  }
  cat("\n                      One-Way MANOVA\n")
  summ.MANOVA <- summary(x$x.mnv, test = test)
  print(summ.MANOVA)
  invisible(x)
}
