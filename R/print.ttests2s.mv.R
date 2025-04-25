#' Prints multiple two-sample t-tests for a multivariate data set
#'
#' Prints the results produced by \code{\link[smsets]{ttests2s.mv}}, consisting
#' of two-sample \emph{t}-tests on more than one response vector with corrected
#' significance levels for multiple comparisons, as offered by \code{p.adjust}.
#' Effects sizes are also displayed.
#'
#' @param x an object of class \code{"ttests2s.mv"}
#' @param ... further arguments passed to or from other methods.
#'
#' @returns {
#' An annotated output of multiple two-sample \emph{t}-tests on more than one
#' response vector with (optionally) corrected significance levels. The argument
#' `x`, invisibly, as for all print methods, is a list of class
#' "\code{ttests2s.mv}". This `print` method provides a user-friendly display
#' of particular elements in `x`:
#' \itemize{
#'   \item A description of the analysis.
#'   \item The data frame analyzed.
#'   \item The labels of the two-level group factor (samples), with an order
#'   determined by the user in the `ttests2s.mv` argument `level1`.
#'   \item The \emph{t}-test results for each response variable; these include:
#'     \itemize{
#'       \item The variable name.
#'       \item Sample means and variances classified by group levels.
#'       \item The value of the \emph{t}-statistic, the degrees of freedom and
#'       the \emph{p}-value.
#'      \item Effect sizes: raw and Hedge's (1981). The units of raw effect
#'      sizes are shown according to the argument `unit =` in `ttests2s.mv`.
#'    }
#'   \item The type of alternative hypothesis for all tests.
#'   \item The method of significance level adjustment for multiple comparisons
#'    used.
#'   }
#' }
#'
#' @references
#' Hedges, L. V. 1981. Distribution theory for Glass’s estimator of effect size
#' and related estimators. _Journal of Educational Statistics_ 6(2): 107–128.
#'
#' @examples
#' data(sparrows)
#' ttests.sparrows <- ttests2s.mv(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE, P.adjust = "holm",
#'                               unit = "mm")
#' print(ttests.sparrows)
#'
#' @import stringr
#' @exportS3Method print ttests2s.mv
print.ttests2s.mv <- function(x, ...) {
  if (x$var.equal == TRUE)
    cat(" Multiple Two Sample t-tests for Multivariate Data\n\n")
  else
    cat(" Multiple Welch's Two Sample t-tests for Multivariate Data\n\n")
  #
  cat("Data:  ", x$data.name, "\n")
  cat("Group levels: (1)", x$levels.group[1], ";  (2)", x$levels.group[2],
      "\n\n")
  fac <- x$data[, names(x$data) %in% c(x$group)]
  fac <- droplevels(fac)
  df <- x$data[,!names(x$data) %in% c(x$group)]
  y <- cbind(fac, df)
  for (i in 1:ncol(df)) {
    cat("Variable: ", names(df)[i], "\n")
    cat("Sample estimates: \n")
    vec.mv <- c(mean(y[fac == x$levels.group[1], i+1], na.rm = TRUE),
                var(y[fac == x$levels.group[1], i+1], na.rm = TRUE),
                mean(y[fac == x$levels.group[2], i+1], na.rm = TRUE),
                var(y[fac == x$levels.group[2], i+1], na.rm = TRUE))
    names(vec.mv) <- c(paste("Mean of", x$levels.group[1]),
                       paste("Variance of", x$levels.group[1]),
                       paste("Mean of", x$levels.group[2]),
                       paste("Variance of", x$levels.group[2]))
    print(round(vec.mv, digits = 2))
    cat("t =", formatC(x$t.list[[i]][1], digits = 4, format = "f"),
        ", df =", formatC(x$t.list[[i]][2],
                          digits = ifelse(x$var.equal == TRUE, 0, 1),
                          format = "f"),
        ", p-value =", formatC(x$t.list[[i]][3], digits = 4, format = "f"),
        "\n")
    cat("Effect size: Raw =",
        formatC(x$t.list[[i]][4], digits = 3, format = "f"), x$unit,
        " ;   Hedges' d = ", formatC(x$t.list[[i]][5], digits = 3, format = "f")
        , "\n\n")
  }
  type.alt <- ifelse(x$alternative == "two.sided",
                     "true difference in means is not equal to 0",
                     ifelse(x$alternative == "greater",
                            "true difference in means is greater than 0",
                            "true difference in means is less than 0"))
  cat("Alternative hypothesis for all tests:", type.alt, "\n")
  if (x$P.adjust != "none") {
    adj.method <- ifelse((x$P.adjust == "fdr") |
                           (x$P.adjust == "BH") |
                           (x$P.adjust == "BY"), toupper(x$P.adjust),
                         stringr :: str_to_title(x$P.adjust))
    cat(paste("P-values adjusted using", adj.method, "method"))
    } else {
    cat("No P-value adjustment made.\n") }
  invisible(x)
}
