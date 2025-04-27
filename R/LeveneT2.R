#' @title Levene's test for two multivariate samples based on Hotelling's
#' \eqn{T^2} test with extra information
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references Curran, J. and Hersh, T. (2021). \emph{Hotelling: Hotelling's T^2
#' Test and Variants}. R package version 1.0-8,
#' <https://CRAN.R-project.org/package=Hotelling>.
#'
#' Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn.
#' Chapman and Hall/CRC.
#'
#' Nel, D.G. and van de Merwe, C.A. (1986). A solution to the multivariate
#' Behrens-Fisher problem. \emph{Comm. Statist. Theor. Meth.}, A15, 12,
#' 3719-3736.
#'
#' @description
#' An R function for the comparison of multivariate variation in two samples,
#' which implements Levene's test based on Hotelling's \eqn{T^2}.
#'
#' @param x A data frame with one two-level factor and \emph{p} response
#' variables.
#' @param group Two-level factor defining groups. It must be one of the columns
#' in \code{x}.
#' @param level1 A character string identifying Sample 1. The string must be one
#' of the factor levels in \code{group}.
#' @param var.equal A logical variable indicating whether to treat the
#' within-sample covariance matrices of absolute deviations around medians for
#' samples 1 and 2 as equal or not. The default is \code{TRUE}. If the
#' within-sample covariance matrices of absolute deviations around medians are
#' not assumed equal (\code{FALSE}), Hotelling's \code{T^2} test is performed
#' using the Nel and van der Merwe's (1986) solution to the multivariate
#' Behrens-Fisher problem as implemented in \pkg{Hotelling} package (Curran and
#' Hersh, 2021).
#'
#' @details
#' \code{LeveneT2} makes use of Hotelling's \eqn{T^2} to test the variation in
#' two multivariate samples. This test is an alternative procedure that should
#' be more robust than Box's test which is known to be rather sensitive to the
#' assumption that the samples are from multivariate normal distributions.
#'
#' In \code{LeveneT2} the data values are transformed into absolute deviations
#' from their respective sample medians
#'
#' \deqn{ADM_{ijk} = |x_{ijk}-M_{jk}|}
#'
#' where
#'
#' \eqn{x_{ijk}} is the value of variable \eqn{X_{k}} for the \eqn{i}th
#' individual in sample \eqn{j}, and
#'
#' \eqn{M_{jk}} is the median of \eqn{X_{k}} in sample \eqn{j}.
#'
#' The unequal variation question between samples \eqn{j = 1} and \eqn{j = 2}
#' becomes a \eqn{T^2}-test for the difference of the mean \eqn{ADM} vectors.
#'
#' @return Returns an object of class \code{"LeveneT2"}, a list containing the
#' following components:
#' \tabular{llllllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{medians} \tab A list containing two vectors. The first vector
#'    \code{medians1} contains the medians for all variables in sample 1 as
#'    declared in parameter \code{level1}, and the second vector holds the
#'    corresponding medians for the other sample. \cr
#'    \code{bygroup.data} \tab A list with two data frames \code{matlevel1} and
#'    \code{matlevel2} containing the original variables for samples 1 and 2
#'    respectively  \cr
#'    \code{absdev.median} \tab A list with two data frames
#'    \code{abs.dev.median1} and \code{abs.dev.median2} containing the absolute
#'    deviations from sample medians for samples 1 and 2, respectively. \cr
#'    \code{LeveneT2.test} \tab A list of class \code{hotelling.test} containing
#'    the list \code{stats} and the scalar \code{pval}, produced by function
#'    \code{\link[Hotelling]{hotelling.test}} implemented in package
#'    \pkg{Hotelling} \cr
#'    \code{var.equal} \tab a logical variable indicating whether the two
#'    variances were treated as being equal \code{TRUE} or not \code{FALSE}. \cr
#'    \code{group} \tab a character string specifying the name of the two-level
#'    factor defining groups.  \cr
#'    \code{levels.group} \tab a vector of length two, showing the two levels in
#'    factor \code{group}.  \cr
#'    \code{data.name} \tab a character string giving the name of the data.  \cr
#'    \code{variables} \tab a character string vector containing the variable
#'    names.  \cr
#'    \code{data} \tab the data frame analyzed.  \cr
#' }
#' The extractor function \code{\link[smsets]{print.LeveneT2}} returns an
#' annotated output of the test.
#'
#' @examples
#' data(sparrows)
#' LeveneT2.sparrows <- LeveneT2(sparrows, group = Survivorship, level1 = "S",
#'                               var.equal = TRUE)
#' # Brief output
#' LeveneT2.sparrows
#'
#' @import Hotelling
#' @importFrom stats median qf pf
#' @export LeveneT2
#
LeveneT2 <- function(x, group, level1, var.equal = TRUE)
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  fac <- droplevels(fac)
  levels.group <- as.character(c(unique(fac[fac == level1]),
                                 unique(fac[fac != level1])))
  df <- x[, !names(x) %in% c(group)]
  # Levene test based on the comparison of means of two multivariate vector
  # consisting of absolute deviations around medians, using Hotelling's T2
  matlevel1 <- df[fac == levels.group[1], ]
  matlevel2 <- df[fac == levels.group[2], ]
  bygroup.data <- list(matlevel1 = matlevel1, matlevel2 = matlevel2)
  vecmedian1 <- apply(matlevel1, 2, median)
  names(vecmedian1) <- names(df)
  vecmedian2 <- apply(matlevel2, 2, median)
  names(vecmedian2) <- names(df)
  medians <- list(medians1 = vecmedian1, medians2 = vecmedian2)
  matabsdev1 <- abs(matlevel1 -
                      matrix(rep(vecmedian1, nrow(matlevel1)),
                             nrow = nrow(matlevel1), byrow = TRUE))
  matabsdev2 <- abs(matlevel2 -
                      matrix(rep(vecmedian2, nrow(matlevel2)),
                             nrow = nrow(matlevel2), byrow = TRUE))
  absdev.median <- list(abs.dev.median1 = as.data.frame(matabsdev1),
                        abs.dev.median2 = as.data.frame(matabsdev2))
  matabsdev.all <- rbind(matabsdev1, matabsdev2)
  matabsdev.all <- data.frame(fac, matabsdev.all)
  names(matabsdev.all)[2:(ncol(df)+1)] <- names(df)
  LeveneT2.test <-
    Hotelling :: hotelling.test(matabsdev1, matabsdev2, var.equal = var.equal)
  results.LeveneT2 <- list(name = "Levene's test based on Hotelling's T2",
                           medians = medians, bygroup.data = bygroup.data,
                           absdev.median = absdev.median,
                           LeveneT2.test = LeveneT2.test,
                           var.equal = var.equal, group = group,
                           levels.group = levels.group,
                           data.name = deparse(substitute(x)),
                           variables = names(df), data = x)
  class(results.LeveneT2) <- "LeveneT2"
  return(results.LeveneT2)
}
