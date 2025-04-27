#' Prints Levene's test based on Hotelling's \eqn{T^2} test
#'
#' Prints the results produced by \code{\link[smsets]{LeveneT2}}, consisting of
#' a Levene's test for two multivariate samples based on Hotelling's \eqn{T^2}
#' test.
#'
#' @param x an object of class \code{"LeveneT2"}.
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default).
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' Displays the results of the comparison of multivariate variation in two
#' samples in which data values are transformed into absolute deviations from
#' their respective sample medians, and mean vectors of absolute deviations are
#' compared using Hotelling's \eqn{T^2} test. The argument `x`, invisibly, as
#' for all print methods, is a list of class "\code{LeveneT2}". This `print`
#' method provides two sorts of output depending on whether the `long` argument
#' is `TRUE` or `FALSE` (the default). The "short" output displays:
#' \itemize{
#'   \item A description of the analysis.
#'   \item The data frame analyzed.
#'   \item The names of responses in the data frame.
#'   \item The labels of the two-level group factor (samples), with an order
#'   determined by the argument `level1` in `LeveneT2`.
#'   \item The value of Hotelling's \eqn{T²}-statistic.
#'   \item The value of the \emph{F}-statistic with its corresponding degrees of
#'       freedom for numerator and denominator. When the within-sample
#'       covariance matrices of absolute deviations around medians are not
#'       assumed equal (`var.equal = FALSE`), these degrees of freedom are
#'       approximated using the Nel and van der Merwe's (1986) solution to the
#'       multivariate Behrens-Fisher problem, as implemented in \pkg{Hotelling}
#'       package (Curran and Hersh, 2021).
#'   \item The P-value.
#'  }
#'
#' In addition to the above information, the "long" output lists:
#' \itemize{
#'    \item Sub-data frames containing the original responses and medians,
#'    separately for each sample.
#'    \item The absolute deviations from sample medians for samples 1 and 2.
#'    \item Vectors of mean absolute deviations around medians for samples 1
#'    and 2, used in Hotelling's \eqn{T²} test.
#' }
#'
#' @references Curran, J. and Hersh, T. (2021). \emph{Hotelling: Hotelling's T^2
#' Test and Variants}. R package version 1.0-8,
#' <https://CRAN.R-project.org/package=Hotelling>.
#'
#' Nel, D.G. and van de Merwe, C.A. (1986). A solution to the multivariate
#' Behrens-Fisher problem. \emph{Comm. Statist. Theor. Meth.}, A15, 12,
#' 3719-3736.
#'
#' @examples
#' data(sparrows)
#' LeveneT2.sparrows <- LeveneT2(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE)
#' # Long output
#' print(LeveneT2.sparrows, long = TRUE)
#'
#' @exportS3Method print LeveneT2
#'
print.LeveneT2 <- function(x, long = FALSE, ...) {
  stopifnot(inherits(x, "LeveneT2"))
  cat(" Comparison of variation for two multivariate samples (Levene's test)")
  cat("\n\n Variation is measured as absolute deviations around group medians")
  cat("\n Hotelling's test compares two vectors of mean absolute deviations")
  cat("\n\n Data: ", x$data.name, "\n")
  cat(" Variables: ", x$variables, "\n")
  cat(" Group levels: (1)", x$levels.group[1], ";  (2)", x$levels.group[2],
      "\n")
  fac <- x$data[, names(x$data) %in% c(x$group)]
  fac <- droplevels(fac)
  df <- x$data[,!names(x$data) %in% c(x$group)]
  y <- cbind(fac, df)
  if (long == TRUE) {
    for (i in 1:2) {
      cat("\nData for group", x$levels.group[i], "\n")
      print(x$bygroup.data[[i]])
      cat("\nMedians of data for group", x$levels.group[i], "\n")
      print(x$medians[[i]])
    }
    for (i in 1:2) {
      cat("\nAbsolute deviations from sample medians for group",
          x$levels.group[i], "\n")
      print(x$absdev.median[[i]])
      cat(" \nVector of mean absolute deviations around medians for group",
          x$levels.group[i], "\n")
      print(round(colMeans(x$absdev.median[[i]]), 2))
    }
  }
  if (x$var.equal == TRUE) {
    cat("\n Levene's test based on Hotelling's T2\n")
    n1 <- length(fac[fac == x$levels.group[1]])
    n2 <- length(fac[fac == x$levels.group[2]])
    p <- df.numF <- ncol(df)
    df.denF <- n1 + n2 - p - 1
    T2 <- x$LeveneT2.test$stat[[1]]
    F.stat <- as.vector(df.denF * T2/((n1 + n2 - 2) * p))
    p.value <- pf(F.stat, df.numF, df.denF, lower.tail = FALSE)
  }
  else {
    cat("\n Levene's test with modified degrees of freedom: Nel and Van der\n")
    cat(" Merwe's (1986) solution to the multivariate Behrens-Fisher problem\n")
    F.stat <- qf(x$LeveneT2.test$pval, x$LeveneT2.test$stats$df[1],
                 x$LeveneT2.test$stats$df[2], lower.tail = FALSE)
  }

  cat(" T2 statistic =", formatC(x$LeveneT2.test$stats$statistic, digits = 4,
                                 format = "f"), "\n", "F =", formatC(F.stat, digits = 4,
                                                                     format = "f"), "\n",
      "Num df =", x$LeveneT2.test$stats$df[1], "\n", "Den df =",
      formatC(x$LeveneT2.test$stats$df[2],
              digits = ifelse(x$var.equal == TRUE, 0, 1), format = "f"), "\n",
      "p-value =", ifelse(x$var.equal == TRUE,
                          formatC(pf(F.stat, df.numF, df.denF,
                                     lower.tail = FALSE), digits = 4, format =
                                    "f"), formatC(x$LeveneT2.test$pval,
                                                  digits = 4, format = "f")),
      "\n")
  invisible(x)
}
