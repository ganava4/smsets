#' @title F approximation of Box's M test
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references
#'
#' Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn. CRC Press.
#'
#' @description
#' An R function which implements an F approximation for testing the homogeneity
#' of covariance matrices by Box's M. This is an alternative approach to the chi
#' square approximation which requires group sample-sizes to be at least 20.
#'
#' @param x A data frame with \eqn{p + 1} columns (one factor and \emph{p}
#' response variables).
#' @param group The classification factor defining \emph{m} samples or groups.
#' It must be one of the columns in \code{x}.
#'
#' @details
#' For \eqn{m} samples, the \eqn{M} statistic is given by the equation
#'
#' \deqn{M = \frac{\prod_{j=1}^{m} |\mathbf{C}_j|^{(n_{j}-1)/2}}{|\mathbf{C}|^{(n-m)/2}}}
#'
#' where
#'
#' \eqn{n_{j}} is the sample size of the \eqn{j}-th sample,
#'
#' \eqn{|\mathbf{C}_j|} is the determinant of the sample covariance matrix
#' for the \eqn{j}th sample,
#'
#' \eqn{|\mathbf{C}|} is the determinant of the pooled covariance matrix,
#'
#' \eqn{n} is the total number of observations.
#'
#' Large values of \eqn{M} provide evidence that the samples are not from
#' populations with the same covariance matrix. In addition to the observed
#' \emph{M}-value itself, the F-approximation involves the sample sizes and the
#' number of variables analyzed. See the reference for details. Box's test is
#' sensitive to deviations from normality in the distribution of the variables.
#'
#' @return Returns an object of class \code{"BoxM.F"}, a list containing the
#' following components:
#' \tabular{lllllllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{Cov.Mat} \tab A list containing the \emph{m} sample covariance
#'    matrices \cr
#'    \code{Cov.pooled} \tab The pooled covariance matrix \cr
#'    \code{BoxM.stat} \tab The approximate F-statistic \cr
#'    \code{F.BoxM} \tab The calculated F-statistic \cr
#'    \code{df.v1} \tab Numerator degrees of freedom for the F statistic \cr
#'    \code{df.v2} \tab Denominator degrees of freedom for the F statistic \cr
#'    \code{Pvalue} \tab P-value for the F statistic  \cr
#'    \code{group} \tab a character string specifying the name of the
#'    classification factor defining groups.  \cr
#'    \code{levels.group} \tab a vector of length \emph{m}, showing the levels
#'    in factor \code{group}.  \cr
#'    \code{data.name} \tab a character string giving the name of the data.  \cr
#'    \code{variables} \tab a character string vector containing the variable
#'    names.  \cr
#'    \code{data} \tab the data frame analyzed.  \cr
#' }
#'
#' @examples
#' data(skulls)
#' resBoxM.F <- BoxM.F(skulls, Period)
#' # Brief output
#' resBoxM.F
#'
#' @import biotools
#' @importFrom stats pf
#' @export BoxM.F
#
BoxM.F <- function(x, group)
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  levels.group <- as.character(unique(fac))
  df <- x[, !names(x) %in% c(group)]
  col.fac <- which(names(x) == group)
  m <- length(unique(fac)) # m, number of levels for group
  p <- ncol(x) - 1         # p, number of variables
  n <- nrow(x)             # n, number of sampling units
  # Box's M test
  logM <- 0
  ni <- rep(NULL, m)
  chitest.boxM <- biotools :: boxM(df, fac)
  Cov.Mat <- chitest.boxM$cov
  Cov.pooled <- chitest.boxM$pooled
  for (i in 1:m) {
    ni[i] <- sum(as.numeric(fac) == i)
    logM <- logM + ((ni[i]-1)/2)*(chitest.boxM$logDet[i])
  }
  logM <- logM -((n-m)*log(det(chitest.boxM$pooled))/2)
  BoxM.stat <- exp(logM)
  names(BoxM.stat) <- "Box's M"
  c1 <- (2*p^2 + 3*p - 1)*(sum(1/(ni-1))-(1/(n-m)))/(6*(p+1)*(m-1))
  c2 <- (p-1)*(p+2)*(sum(1/(ni-1)^2)-(1/(n-m)^2))/(6*(m-1))
  v1 <- p*(p+1)*(m-1)/2
  v2 <- (v1+2)/(c2-c1^2)
  b <-  (1 - c1 - v1/v2)/v1
  b
  b1 <- (1 - c1 - 2/v2)/v2
  F.BoxM <- ifelse(c2 > c1^2, -2*b*logM, (-2*b1*v2*logM)/(v1+2*b1*v1*logM))
  F.BoxM
  Pvalue <- pf(F.BoxM, v1, v2, lower.tail = FALSE)
  Pvalue
  results.BoxM.F <- list(name =
                           "Box's M-test for Homogeneity of Covariance Matrices",
                         Cov.Mat = Cov.Mat, Cov.pooled = Cov.pooled,
                         BoxM.stat = BoxM.stat, F.BoxM = F.BoxM,
                         df.v1 = v1, df.v2 = v2, Pvalue = Pvalue,
                         group = group, levels.group = levels.group,
                         data.name = deparse(substitute(x)),
                         variables = names(df), data = x)
  class(results.BoxM.F) <- "BoxM.F"
  return(results.BoxM.F)
}
