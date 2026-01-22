#' @title Robust Levene's Tests for the Comparison of Variation of m Samples in Multivariate Data
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references
#' Ben-Shachar, M., Lüdecke, D., and Makowski, D. (2020). effectsize: Estimation
#' of Effect Size Indices and Standardized Parameters. \emph{Journal of Open
#' Source Software}, 5(56), 2815. doi: 10.21105/joss.02815
#'
#' Fox, J., and Weisberg, S. (2019). \emph{An R Companion to Applied Regression},
#' Third edition. Sage, Thousand Oaks CA. https://www.john-fox.ca/Companion/.
#'
#' Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn.
#' Chapman and Hall/CRC.
#'
#' Welch, B.L. (1951). On the comparison of several mean values: an alternative
#' approach. \emph{Biometrika}, 38, 330–336. doi:10.2307/2332579.
#'
#' @description
#' Performs Levene's tests for \emph{m} samples on \emph{p} responses, based on
#' (univariate) One-Way ANOVAs and One-Way MANOVAs applied to absolute
#' differences around medians. Significance levels of the univariate tests of
#' variation can be corrected using any of the adjustment methods for multiple
#' comparisons offered by \code{\link[stats]{p.adjust}}. Effects sizes are also
#' computed with respect to the One-Way ANOVAs.
#'
#' @param x a data frame containing a factor with \emph{m} levels and \emph{p}
#' response variables.
#' @param group a factor with \emph{m} levels defining samples. It must be one
#' of the columns in \code{x}.
#' @param var.equal a logical variable indicating whether to treat the \emph{m}
#' variances of absolute deviations around medians (the variances of the measure
#'  of variation among samples!) as being equal for the One-Way ANOVAs. If
#' \code{TRUE} then a simple F test for the equality of means is performed. If
#' \code{FALSE} the Welch (1951) approximation to the degrees of freedom is
#' used, as implemented in \code{\link[stats]{oneway.test}}.
#' @param P.adjust p-value correction method of univariate Levene's tests
#' (One-Way ANOVAs), a character string. Can be abbreviated. See 'Details'.
#'
#' @details
#' This function focuses on robust Levene's tests, both univariate and
#' multivariate, for the comparison of variation among \emph{m} samples in
#' multivariate data. These tests can be chosen as alternatives to Box's test
#' which is sensitive to deviations from normality. The application of
#' Levene's test one variable at a time from a set of \emph{p} variables can be
#' computed by repeating \emph{p} times \pkg{car}'s package function
#' \code{\link[car]{leveneTest}} (Fox and Weisberg 2019), when
#' \code{center = median}. However, there are \emph{p} univariate Levene's tests
#'  possible, each one consisting of one-way ANOVAs applied to the absolute
#' deviations around medians. Therefore, the p-values produced in the ANOVAs can
#'  be subject to corrections for multiple testing, depending on the number of
#' response variables analyzed. The methods implemented in \code{P.adjust} are
#' the same as those contained in the \code{p.adjust.methods}:
#' \code{"bonferroni"}, \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
#' \code{"BH"}, (Benjamini-Hochberg) or its alias \code{"fdr"} (False Discovery
#' Rate), and \code{"BY"} (Benjamini & Yekutieli). The default pass-through
#' option (\code{"none"}) is also included. Four measures of effect size are
#' also computed with respect to the univariate  F tests, which are interpreted
#' as effect sizes of variation among samples. User-friendly summaries of all
#' analyses (including the multivariate Levene's test) can be invoked using the
#' \code{print} method for this function.
#'
#' @return Returns an list of class \code{"Levenetestsms.mv"}, a list
#' containing the following components:
#' \tabular{lllllllllllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{medians} \tab A \eqn{m \times p} matrix; the cell
#'    (\emph{m},\emph{p}) contains the median of the \emph{p}-th response in
#'    sample \emph{m}. \cr
#'    \code{absdev_medians} \tab A list containing \emph{m} data frames, one
#'    data frame for each level of \code{group}, and each data frame having
#'    \emph{p} columns containing the absolute deviations around the \emph{m}-th
#'    sample median. \cr
#'    \code{df_absdev} \tab A data frame containing the absolute deviations
#'    around medians, seen as a compact version of \code{absdev_medians}. \cr
#'    \code{means_absdev} \tab A \eqn{m \times p} matrix; the cell
#'    (\emph{m},\emph{p}) contains the mean absolute deviation around the median
#'    of the \emph{p}-th response in sample \emph{m}. \cr
#'    \code{vars_absdev} \tab A \eqn{m \times p} matrix; the cell
#'    (\emph{m},\emph{p}) contains the variance of absolute deviations around
#'     the median of the \emph{p}-th response in sample \emph{m}. \cr
#'    \code{OneWayANOVAs} \tab A list containing the results of the \emph{p}
#'    tests for equal means of absolute deviations around medians in a one-way
#'    layout. Each element in the list is basically the result of
#'    \code{\link[stats]{oneway.test}}, but the p-values have been possibly
#'    recomputed as a consequence of the \code{P.adjust} method chosen. \cr
#'    \code{ANOVATables} \tab A list containing \emph{p} analysis of variance
#'    tables produced by \code{anova.lm}, each table corresponding to a one-way
#'    analysis of variance for the comparison of \emph{m}-samples on the
#'    \emph{p}-th response variable. Each element in the list is basically the
#'    result of \code{\link[stats]{anova.lm}}, but the p-values have been
#'    possibly recomputed as a consequence of 1) the \code{P.adjust} method
#'    chosen, and/or 2) the assumption of equal variance of absolute deviations
#'    around medians is \code{FALSE}. \cr
#'    \code{var.equal} \tab A logical variable indicating whether the two
#'    variances were treated as being equal \code{TRUE} or not \code{FALSE}.
#'    \cr
#'    \code{P.adjust} \tab A character string indicating the correction method
#'    chosen. \cr
#'    \code{Eff_sizes} \tab A list of length \emph{p} containing four effect
#'    size measures for an F-test in one-way ANOVA, and their respective 95%
#'    confidence intervals. Those measures are η², ω², ϵ² and Cohen's f, as
#'    implemented in the \pkg{effectsize} package (Ben-Shachar et al. 2020).
#'    When \code{var.equal = FALSE} these effect sizes are approximations.\cr
#'    \code{OWM_absdev} \tab A list of class "\code{manova}" containing  the
#'    results of the One-Way MANOVA applied to the absolute deviations around
#'    medians, i.e., the multivariate Levene's test. \cr
#'    \code{group} \tab A character string specifying the name of the
#'    \emph{m}-level factor defining samples. \cr
#'    \code{levels.group} \tab A vector of length \emph{m} showing the levels in
#'    factor \code{group}. \cr
#'    \code{variables} \tab A vector of length \emph{p} showing the names of
#'    response variables. \cr
#'    \code{data.name} \tab A character string giving the name of the data. \cr
#'    \code{data} \tab The data frame analyzed.  \cr
#' }
#'
#' The extractor function \code{\link[smsets]{print.Levenetestsms.mv}} returns
#' an annotated output of the univariate Levene tests and, optionally, the
#' multivariate Levene's test.
#'
#' @examples
#' data(skulls)
#' res.Levenems.mv <- Levenetestsms.mv(skulls, Period, var.equal = TRUE,
#'                                     P.adjust = "bonferroni")
#' res.Levenems.mv
#'
#' @import effectsize
#' @importFrom stats p.adjust oneway.test lm anova median var as.formula
#' summary.manova
#' @export Levenetestsms.mv
Levenetestsms.mv <- function(x, group, var.equal = FALSE, P.adjust = "none")
{
# Levene's tests based on absolute differences around medians
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  fac <- droplevels(fac)
  m <- length(unique(fac))
  levels.group <- as.character(unique(fac))
  facord <- factor(fac, levels = levels.group)
  df <- x[, !names(x) %in% c(group)]
# Split the data frame into a list of data frames
  absdev_medians <- split(df, facord)
  p <- ncol(df)
  medians <- matrix(NA, nrow = m, ncol = p)
  for (i in 1:m) {
    matlist <- as.matrix(absdev_medians[[i]])
    medians[i, ] <- apply(matlist, 2, median)
  }
  rownames(medians) <- levels.group
  colnames(medians) <- names(df)
  for (i in 1:m) {
    for (j in 1:nrow(absdev_medians[[i]])) {
      for (k in 1:p) {
        absdev_medians[[i]][j, k] <- abs(df[j, k] - medians[i, k])
      }
    }
  }
  means_absdev <-   vars_absdev <- matrix(NA, nrow = m, ncol = p)
  for (i in 1:m) {
    matabsdev <- as.matrix(absdev_medians[[i]])
    means_absdev[i, ] <- apply(matabsdev, 2, mean)
    vars_absdev[i, ] <- apply(matabsdev, 2, var)
  }
  rownames(means_absdev) <- rownames(vars_absdev) <- levels.group
  colnames(means_absdev) <- colnames(vars_absdev) <- names(df)
  df_absdev <- do.call(rbind, absdev_medians)
  df_absdev <- cbind(fac, df_absdev)
  rownames(df_absdev) <- rownames(df)
  names(df_absdev)[1] <- group
  df_absdev <- as.data.frame(df_absdev)
  OneWayANOVAs <- vector("list", p)
  ANOVATables <- vector("list", p)
  P.value.adj <- numeric(p)
  Eff_sizes <- vector("list", p)
  for (k in 1:p) {
    y <- names(df[k])
    xvar <- group
    formula_string <- paste(y, xvar, sep = "~")
    OneWayANOVAs[[k]] <- oneway.test(as.formula(formula_string),
                                     data = df_absdev, var.equal = var.equal)
    ANOVATables[[k]] <- anova(lm(as.formula(formula_string), data = df_absdev))
    P.value.adj[k] <- OneWayANOVAs[[k]]$p.value
    OneWayANOVAs[[k]]$p.value <- p.adjust(P.value.adj, method = P.adjust)[k]
    ANOVATables[[k]]$Df <- round(OneWayANOVAs[[k]]$parameter, 2)
    ANOVATables[[k]]$'F value'[1] <- OneWayANOVAs[[k]]$statistic
    ANOVATables[[k]]$'Pr(>F)'[1] <- P.value.adj[k]
    MAT_ES <- matrix(NA, nrow = 4, ncol = 3)
    ETA2 <- effectsize :: eta_squared(OneWayANOVAs[[k]])[, c(1, 3, 4)]
    MAT_ES[1, ] <- as.matrix(ETA2)
    OMEGA2 <- effectsize :: omega_squared(OneWayANOVAs[[k]])[, c(1, 3, 4)]
    MAT_ES[2, ] <- as.matrix(OMEGA2)
    EPSILON2 <- effectsize :: epsilon_squared(OneWayANOVAs[[k]])[, c(1, 3, 4)]
    MAT_ES[3, ] <- as.matrix(EPSILON2)
    COHENSF <- effectsize :: cohens_f(OneWayANOVAs[[k]])[, c(1, 3, 4)]
    MAT_ES[4, ] <- as.matrix(COHENSF)
    rownames(MAT_ES) <- c("η\u00B2", "ω\u00B2", "ϵ\u00B2", "Cohen's f")
    colnames(MAT_ES) <- c("E.S. Measure", "95%-LCL", "95%-UCL")
    Eff_sizes[[k]] <- MAT_ES
  }
  names(OneWayANOVAs) <- names(ANOVATables) <- names(Eff_sizes) <- names(df)
  if (P.adjust != "none") {
    for (k in 1:p) {
     ANOVATables[[k]]$'Pr(>F)'[1] <- p.adjust(P.value.adj, method = P.adjust)[k]
    }
  }
  Mat <- as.matrix(df_absdev[, -1])
  xvar <- fac
  OWM_absdev <- with(df_absdev, manova(Mat ~ fac))
  results.Levms <- list(name = "m-sample Levene Tests for Multivariate Data",
                        medians = medians, absdev_medians = absdev_medians,
                        df_absdev = df_absdev, means_absdev = means_absdev,
                        vars_absdev = vars_absdev, OneWayANOVAs = OneWayANOVAs,
                        ANOVATables = ANOVATables, var.equal = var.equal,
                        P.adjust = P.adjust, Eff_sizes = Eff_sizes,
                        OWM_absdev = OWM_absdev, group = group,
                        levels.group = levels.group, variables = names(df),
                        data.name = deparse(substitute(x)), data = x)
  class(results.Levms) <- "Levenetestsms.mv"
  return(results.Levms)
}
