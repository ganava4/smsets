#' @title Prints multiple Levene's tests of multivariate data.
#'
#' @description
#' Prints the results produced by \code{\link[smsets]{Levenetestsms.mv}},
#' consisting of \emph{m}-sample Levene's tests computed from one-way ANOVAs
#' applied to absolute differences around medians for \emph{p} responses. It
#' optionally produces a single multivariate Levene's test which is basically a
#' one-way MANOVA of absolute differences around medians.
#'
#' @param x an object of class "Levenetestsms.mv".
#' @param format a character string specifying the one-way ANOVA results are
#' formatted, must be one of \code{"oneway.test"} (default), \code{"anova.lm"}
#' or \code{"both"}.
#' @param EffectSize a logical variable. If \code{TRUE} (default), the measures
#' of effect size \eqn{\eta^2}, \eqn{\omega^2}, \eqn{\epsilon^2}, and Cohen's f
#' are returned.
#' @param multivariate a logical variable. If \code{TRUE}, the multivariate
#' Levene's test is returned. The test statistic chosen can be set with the
#' optional argument \code{mv_statistic}.
#' @param mv_statistic a character string. The name of the test statistic to be
#' used in the multivariate Levene's test (any of the four tests implemented in
#' \code{\link[stats]{summary.manova}}). Pillai's test is the default. Partial
#' matching is used so the name can be abbreviated.
#' @param long A logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default). The additional objects
#' returned are three matrices: 1) matrix of medians for the original data
#' classified by sample and response variable, 2) matrix of mean absolute
#' deviations around means, and 3) matrix of variances of absolute deviations
#' around means.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns {
#' An annotated output of \emph{m}-sample Levene's tests computed from one-way
#' ANOVAs applied to absolute differences around medians for \emph{p} responses,
#'  with (optionally) corrected significance levels. The argument \code{x},
#' invisibly, as for all print methods, is a list of class
#' "\code{Levenetestsms.mv}". This \code{print} method provides a user-friendly
#' display of particular elements in \code{x}:
#' \itemize{
#'   \item A description of the analysis.
#'   \item The data frame analyzed
#'   \item The labels of the group factor with \emph{m} levels (samples).
#'   \item The univariate Levene's tests based on the results of one-way ANOVAs
#'   for each response variable; These results consist of the \emph{F}-tests
#'   displayed in up to two different output formats corresponding to the
#'   formatted results produced by the conventional functions
#'   \code{\link[stats]{oneway.test}} and \code{\link[stats]{anova.lm}}.
#'   P-values are recomputed whenever \code{P.adjust} method is different
#'   from \code{"none"}.
#'   }
#' In addition to the above information, the \code{long = TRUE} output lists:
#' \itemize{
#'       \item Sample medians classified by group levels and variables.
#'       \item Means and variances of sample absolute deviations from the median
#'       classified by group levels and variables.
#' }
#' If \code{multivariate = TRUE}, the multivariate Levene's test is appended,
#' based on the test statistic specified in \code{mv_statistic}. If
#' \code{multivariate = TRUE} and \code{mv_statistic} is omitted, Pillai's
#' statistic is chosen by default.
#' }
#'
#' @examples
#' data(skulls)
#' res.Levenems.mv <- Levenetestsms.mv(skulls, Period, var.equal = TRUE,
#'                                     P.adjust = "bonferroni")
#' print(res.Levenems.mv, format = "both", multivariate = TRUE, long = TRUE)
#'
#' @import stringr crayon
#' @exportS3Method print Levenetestsms.mv
print.Levenetestsms.mv <- function(x, format = "oneway.test", EffectSize = TRUE,
                                   multivariate = FALSE,
                                   mv_statistic = c("Pillai", "Wilks",
                                                    "Hotelling-Lawley", "Roy"),
                                   long = FALSE, ...) {
  stopifnot(inherits(x, "Levenetestsms.mv"))
  cat("           m-sample Levene's tests for homogeneity of variance in multivariate data\n")
  cat("             ")
  if (x$var.equal == TRUE) {
    cat("Testing variation using F-tests via absolute deviations from medians\n")
  } else {
    cat("Testing variation using Welch F-tests via absolute deviations from medians\n")
}
  cat("\nData: ", x$data.name, "\n")
  cat("Variables: ", x$variables, "\n")
  cat("Group levels: ", x$levels.group, "\n")
  fac <- x$data[, names(x$data) %in% c(x$group)]
  fac <- droplevels(fac)
  df <- x$data[,!names(x$data) %in% c(x$group)]
  y <- cbind(fac, df)
  if (long == TRUE) {
    cat("\nSummary statistics\n")
    cat("Matrix of medians for the original variables\n")
    print(x$medians, digits = 2)
    cat(" \nMatrix of mean absolute deviations around medians\n")
    print(x$means_absdev, digits = 2)
    cat(" \nMatrix of variances of absolute deviations around medians\n")
    print(x$vars_absdev, digits = 2)
  }
    cat("\nIndividual Levene's tests, one for each variable.")
    if (x$P.adjust != "none") {
      adj.method <- ifelse((x$P.adjust == "fdr") |
                             (x$P.adjust == "BH") |
                             (x$P.adjust == "BY"), toupper(x$P.adjust),
                             stringr :: str_to_title(x$P.adjust))
      cat(crayon :: underline("\nP-values adjusted using "))
      cat(crayon :: underline(adj.method))
      cat(crayon :: underline(" method.\n\n"))
    } else {
      cat(crayon :: underline("No P-value adjustments made.\n\n"))
    }
    if (format == "oneway.test" || format == "both") {
      cat("Format: \"oneway.test\"\n")
      if (x$var.equal == FALSE) {
     cat("(not assuming equal variance of absolute deviations around medians)")
      }
      for (k in 1:length(x$variables)) {
        x$OneWayANOVAs[[k]]$method <-
          "Levene's test for Homogeneity of Variance (center = median)"
        print(x$OneWayANOVAs[[k]])
      }
    }
    if (format == "anova.lm" || format == "both") {
      cat("\nFormat: \"anova.lm\" ")
      if (x$var.equal == FALSE) {
    cat("(not assuming equal variance of absolute deviations around medians)")
      }
      cat("\n\n")
      for (k in 1:length(x$variables)) {
        attributes(x$ANOVATables[[k]])$heading[1] <-
          "Levene's test for Homogeneity of Variance (center = median)\n"
        print(x$ANOVATables[[k]])
        cat("\n\n")
      }
    }
    if (EffectSize == TRUE) {
      cat("\nEffect size (E.S.) of variation for each variable.\n")
cat("For one-way between subjects designs, partial eta/omega/epsilon squared are
equivalent to eta/omega/epsilon squared. Returning eta/omega/epsilon squared.\n\n")
      for (k in 1: length(x$variables)) {
        cat("Response:", x$variables[k], "\n")
        print(round(x$Eff_sizes[[k]], digits = 3))
        cat("\n")
      }
    }
    if (EffectSize == TRUE && x$var.equal == FALSE) {
        cat("Effect sizes are approximated as a consequence of the assumption of unequal variances\n")
    }
    if (multivariate == "TRUE") {
        cat("\nMultivariate Levene's test\n\n")
        SUMM <- summary.manova(x$OWM_absdev, test = mv_statistic)
        dimnames(SUMM$stats)[[1]][1] <- x$group
        print(SUMM)
    }
  invisible(x)
}
