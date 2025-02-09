#' Prints Levene's test based on Hotelling's \eqn{T^2} test
#'
#' Prints the results produced by \code{\link[smsets]{LeveneT2}}, consisting of
#' Levene's test for two multivariate samples based on Hotelling's \eqn{T^2}
#' test
#'
#' @param x an object of class \code{"LeveneT2"}
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default)
#' @param ... further arguments passed to or from other methods.
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
