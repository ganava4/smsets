#' Prints van Valen's test
#'
#' Displays the results of van Valen's test produced by the \code{VanValen}
#' function and, optionally, the matrices involved in the calculations.
#'
#' @param x an object of class `VanValen`.
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default)
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' Displays the results of van Valen's test produced by the \code{VanValen}
#' function. The argument `x`, invisibly, as for all print methods, is a list
#' of class "\code{VanValen}". This `print` method provides two sorts of
#' output depending on whether the `long` argument is `TRUE` or `FALSE` (the
#' default). The "short" output displays:
#' \itemize{
#'   \item A two-line heading describing the analysis.
#'   \item The data frame analyzed.
#'   \item The variables used for the comparison of samples.
#'   \item The labels of the two-level group factor (samples), with an order
#'   determined by the user in the argument `level1` of `VanValen`.
#'   \item The value of the \emph{t}-statistic, the degrees of freedom and
#'       the \emph{p}-value.
#'   \item The type of alternative hypothesis for the _t_-test.
#' }
#'
#' In addition to the above information, the "long" output lists:
#' \itemize{
#'    \item Sub-data frames containing the standardized data, separately for
#'    each sample.
#'    \item The sample medians for the standardized data, samples 1 and 2.
#'    \item Sub-data frames containing the deviations from sample medians for
#'    the standardized values, separately for each sample.
#'    \item Sub-data frames containing the pooled distances (_d_'s), separately
#'    for each sample. These two samples of _d_-values are compared by a
#'    _t_-test.
#'    \item The means and variances for each sample of _d_-values.
#' }
#'
#' @examples
#' data(sparrows)
#' res.VanValen <- VanValen(sparrows, "Survivorship", "S",
#'                          alternative = "less", var.equal = TRUE)
#' # Long output
#' print(res.VanValen, long = TRUE)
#'
#' @exportS3Method print VanValen
print.VanValen <- function(x, long = FALSE, ...) {
  stopifnot(inherits(x, "VanValen"))
  cat(" Comparison of variation for two multivariate samples (Van Valen's test)")
  cat("\n Variation measured as deviations of standardized data around medians")
  cat("\n\n Data: ", x$data.name, "\n")
  cat(" Variables: ", x$variables, "\n")
  cat(" Group levels: (1)", x$levels.group[1], ";  (2)", x$levels.group[2],
      "\n\n")
  fac <- x$data[, names(x$data) %in% c(x$group)]
  fac <- droplevels(fac)
  df <- x$data[,!names(x$data) %in% c(x$group)]
  y <- cbind(fac, df)
  if (long == TRUE) {
    for (i in 1:2) {
      cat("\nStandardized data for group", x$levels.group[i], "\n")
      print(x$std.data[[i]])
      cat("\nMedians of standardized data for group", x$levels.group[i], "\n")
      print(x$medians.std[[i]])
    }
    for (i in 1:2) {
      cat("\nDeviations from sample medians for standardized values in group",
          x$levels.group[i], "\n")
      print(x$dev.median[[i]])
    }
    for (i in 1:2) {
      cat("\nd's computed from standardized values around the median for group",
          x$levels.group[i], "\n")
      print(x$d.list[[i]])
      cat("\nMean of d's for group", x$levels.group[i], ":", x$means.d[[i]])
      cat("\nVariance of d's for group", x$levels.group[i], ":", x$vars.d[[i]],
          "\n")
    }
  }
  if (x$var.equal == TRUE)
    cat("\n Van Valen's test based on a t-test of d-values\n")
  else
    cat("\n Van Valen's test based on a Welch t-test of d-values\n")
  cat(" t =", formatC(x$t.vec[1], digits = 4, format = "f"),
      ", df =", formatC(x$t.vec[2],
                        digits = ifelse(x$var.equal == TRUE, 0, 1),
                        format = "f"),
      ", p-value =", formatC(x$t.vec[3], digits = 4, format = "f"),
      "\n")
  type.alt <- ifelse(x$alternative == "two.sided",
                     "true difference in means is not equal to 0",
                     ifelse(x$alternative == "greater",
                            "true difference in means is greater than 0",
                            "true difference in means is less than 0"))
  cat(" Alternative hypothesis:", type.alt, "\n")
  invisible(x)
}
