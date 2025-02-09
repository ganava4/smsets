#' Multiple two-sample t-tests for multivariate data
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references
#' Hedges, L. V. 1981. Distribution theory for Glass’s estimator of effect size
#' and related estimators. _Journal of Educational Statistics_ 6(2): 107–128.
#'
#' @description
#' Performs multiple two-sample t-tests on more than one response vector with
#' corrected significance levels using any of the adjustment methods for
#' multiple comparisons offered by \code{p.adjust}. Effects sizes are also
#' computed.
#'
#' @param x A data frame with one two-level factor and _p_ response variables.
#' @param group Two-level factor defining groups. It must be one of the columns
#' in \code{x}.
#' @param level1 A character string identifying Sample 1. The string must be one
#' of the factor levels in \code{group}.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. You can specify just the initial letter.
#' @param var.equal a logical variable indicating whether to treat the two
#' variances as being equal. If \code{TRUE} then the pooled variance is used to
#' estimate the variance otherwise the Welch (or Satterthwaite) approximation to
#' the degrees of freedom is used.
#' @param P.adjust p-value correction method, a character string. Can be
#' abbreviated.
#' @param unit Physical units of the response variable useful to fully
#' characterize raw effect sizes.
#'
#' @details
#' This function extends the univariate \code{t.test} for the comparison of mean
#' values for two samples, when more than one variable is involved in the data
#' analysis, so that type one error rates ("false significances") in a series of
#' univariate t-tests are adjusted according to the number of response
#' variables analyzed. The pairwise comparisons between the two levels in
#' \code{group} with corrections for multiple testing are made over more than
#' one response vector thus, the function is a variation of
#' \code{\link[stats]{pairwise.t.test}}.
#'
#' The methods implemented are the same as those contained in the
#' \code{p.adjust.methods} for \code{p.adjust}: \code{"bonferroni"},
#' \code{"holm"}, \code{"hochberg"}, \code{"hommel"}, \code{"BH"}
#' (Benjamini-Hochberg) or its alias \code{"fdr"} (False Discovery Rate), and
#' \code{"BY"} (Benjamini & Yekutieli). The default pass-through option
#' (\code{"none"}) is also included.
#'
#' @return Returns an object of class \code{"ttests2s.mv"}, a list containing
#' the following components:
#' \tabular{llllllllllll}{
#'    \code{name} \tab A character string describing the function \cr
#'    \code{t.list} \tab A list containing _p_ vectors of length 5, each vector
#'    having the computed t-statistic, the degrees of freedom for the
#'    t-statistic, the adjusted p-value for the test, the raw effect size
#'    estimator: \eqn{\bar{x}_1 - \bar{x}_2}, and the post hoc effect size
#'    estimator recommended by Hedges (1981), analogous to Cohen's _d_, given
#'    by \eqn{|\bar{x}_1 - \bar{x}_2| / \hat{\sigma}}. Here \eqn{\hat{\sigma}
#'    = \sqrt{MSE}} where \eqn{MSE} is mean squared error, the estimator
#'    of the variance for the difference of means \eqn{\bar{x}_1 - \bar{x}_2}.
#'    \cr
#'    \code{alternative} \tab A character string specifying the alternative
#'    hypothesis chosen. \cr
#'    \code{var.equal} \tab A logical variable indicating whether the two
#'    variances were treated as being equal \code{TRUE} or not \code{FALSE}.
#'    \cr
#'    \code{P.adjust} \tab A character string indicating the correction method
#'    chosen \cr
#'    \code{raw.ES} \tab The raw effect size (scalar) expressed in the
#'    pre-specified \code{unit}s \cr
#'    \code{unit} \tab A character string indicating the \code{unit}s chosen \cr
#'    \code{Hedges.d} \tab The post hoc effect size Hedges' estimator (scalar)
#'    \cr
#'    \code{group} \tab A character string specifying the name of the two-level
#'    factor defining groups. \cr
#'    \code{levels.group} \tab A vector of length two showing the two levels in
#'    factor \code{group}. \cr
#'    \code{data.name} \tab A character string giving the name of the data. \cr
#'    \code{data} \tab the data frame analyzed. \cr
#' }
#'
#' The extractor function \code{\link[smsets]{print.ttests2s.mv}} returns an
#' annotated output of each t-test and effect size estimation.
#'
#' @examples
#' data(sparrows)
#' ttests.sparrows <- ttests2s.mv(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE, P.adjust = "bonferroni",
#'                               unit = "mm")
#' ttests.sparrows
#'
#' @importFrom stats p.adjust t.test var
#' @export ttests2s.mv
#'
ttests2s.mv <- function(x, group, level1, alternative = "two.sided",
                        var.equal = FALSE, P.adjust = "none", unit = "units")
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  fac <- droplevels(fac)
  df <- x[,!names(x) %in% c(group)]
  y <- cbind(fac, df)
  t.list <- vector(mode = 'list', length = ncol(y)-1)
  P.value.adj <- rep(NULL, ncol(df))
  for (i in 1:ncol(df)) {
    tstat <- t.test(df[fac == level1, i], df[fac != level1, i],
                    alternative = alternative,
                    var.equal = var.equal)$statistic
    df.t <- t.test(df[fac == level1, i], df[fac != level1, i],
                   alternative = alternative,
                   var.equal = var.equal)$parameter
    P.value <- t.test(df[fac == level1, i], df[fac != level1, i],
                      alternative = alternative,
                      var.equal = var.equal)$p.value
    P.value.adj[i] <- P.value
    std.err <- t.test(df[fac == level1, i], df[fac != level1, i],
                      alternative = alternative,
                      var.equal = var.equal)$stderr
    means.g12 <- t.test(df[fac == level1, i], df[fac != level1, i],
                      alternative = alternative,
                      var.equal = var.equal)$estimate
    raw.ES <- means.g12[1] - means.g12[2]
    names(raw.ES) <- "Effect size"
    Hedges.d <- abs(raw.ES) / std.err
    names(Hedges.d) <- "Hedges' d"
    t.list[[i]] <- c(tstat, df.t, P.value = P.value, raw.ES, Hedges.d)
  }
  if (P.adjust != "none") {
    for (i in 1:ncol(df)) {
      t.list[[i]]["P.value"] <- p.adjust(P.value.adj, method = P.adjust)[i]
    }
  }
  levels.group <- c(level1, levels(fac)[levels(fac) != level1])
  names(t.list) <- names(df)
  results.ts <- list(name = "Multiple two-sample t-tests for multivariate data",
                     t.list = t.list, alternative = alternative,
                     var.equal = var.equal, P.adjust = P.adjust,
                     raw.ES = raw.ES, unit = unit, Hedges.d = Hedges.d,
                     group = group, levels.group = levels.group,
                     data.name = deparse(substitute(x)), data = x)
  class(results.ts) <- "ttests2s.mv"
  return(results.ts)
}
