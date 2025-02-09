#' @title Tests in One-way MANOVA with extra information
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn. CRC Press.
#'
#' @description
#' An R function to test the difference of mean vectors among the levels of a
#' single factor with respect to \emph{p} response variables. Sum of squares and
#' cross-products matrices involved in the MANOVA can be optionally displayed.
#' Test statistics produced are the same as those implemented in
#' \code{summary.manova}
#'
#' @param x A data frame with one factor and \emph{p} response variables.
#' @param group Factor defining groups. It must be one of the columns
#' in \code{x}.
#'
#' @details
#' This function is a simplified version of \code{manova}, focusing in
#' multivariate analysis of variance for one single factor with respect to
#' \emph{p} responses. The \code{print} method in \code{OnewayMANOVA} is similar
#' to that in \code{summary.manova}, producing the same approximate F tests in
#' the one-way MANOVA. A simplified printout of the sums of squares and product
#' matrices involved in the analysis can optionally be chosen.
#'
#' @return Returns an object of class \code{"OnewayMANOVA"}, a list containing
#' the following components:
#' \tabular{llllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{T} \tab The total sum of squares and cross-product matrix, defined
#'    as \eqn{\mathbf{T} = \mathbf{B} + \mathbf{W}}, with \eqn{\mathbf{B}} and
#'    \eqn{\mathbf{W}} described below. \cr
#'    \code{W} \tab The within-sample or residual sum of squares and
#'    cross-product matrix. \cr
#'    \code{B} \tab The between-sample sum of squares and cross-product matrix
#'    \cr
#'    \code{x.mnv} \tab An object of class "manova" (and some other classes)
#'    produced by function \code{manova}, to be passed as argument in
#'    \code{summary.OnewayMANOVA} in order to produce the approximate F-tests.
#'    \cr
#'    \code{group} \tab A character string specifying the name of the factor
#'    defining groups.  \cr
#'    \code{levels.group} \tab A vector showing the levels in factor
#'    \code{group}. \cr
#'    \code{data.name} \tab A character string giving the name of the data. \cr
#'    \code{variables} \tab A character string vector containing the variable
#'    names.  \cr
#'    \code{data} \tab The data frame analyzed.  \cr
#' }
#'
#' @examples
#' data(skulls)
#' res.MANOVA <- OnewayMANOVA(skulls, group = Period)
#' # Brief output
#' res.MANOVA
#'
#' @importFrom stats aggregate manova
#' @export OnewayMANOVA
OnewayMANOVA <- function(x, group)
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  levels.group <- as.factor(unique(fac))
  df <- x[, !names(x) %in% c(group)]
  col.fac <- which(names(x) == group)
  n.levels <- length(unique(x[, col.fac]))
  n.vars <- ncol(x) - 1
  n <- nrow(x)
  M <- rbind(matrix(rep(colMeans(x[, -col.fac]), n), nrow = n, byrow = TRUE))
  # Matrix of deviations around the overall means
  DOM <- as.matrix(x[, -col.fac]) - M
  # Total sum of squares and cross products matrix
  T <- crossprod(DOM)
  T
  # Setting-up calculations of deviations around within-sample means matrix
  # means
  MG <- with(x, aggregate(df, by = list(Group = x[ , col.fac]), FUN = mean))
  MG
  list.ind <- vector(mode = 'list', length = n.levels)
  length.ind <- vector(mode = "numeric", length = n.levels)
  names(list.ind) <- levels(x[, col.fac])
  Mat.wsmeans <- matrix(rep(0, n*n.vars), nrow = n, byrow = TRUE)
  for (i in 1:n.levels) {
    list.ind[[i]] <- which(x[, col.fac] == levels(x[, col.fac])[i])
  }
  for (i in 1:n.levels) {
    for (j in list.ind[[i]]) {
      Mat.wsmeans[j, ] <- as.matrix(MG[i, -col.fac])
    }
  }
  colnames(Mat.wsmeans) <- names(x)[-col.fac]
  # Matrix of deviations around the within-sample means
  DWM <- as.matrix(x[, -col.fac]) - Mat.wsmeans
  # Within-sample sum of squares and cross products matrix
  W <- crossprod(DWM)
  # Between-sample sum of squares and cross products matrix
  B <- T - W
  # Comparing multivariate means (One-way MANOVA)
  Mat <- as.matrix(x[, -col.fac])
  form <- paste("Mat ~ ", group, sep = "")
  x.mnv <- with(x, manova(as.formula(form), data=x))
  names(x.mnv$xlevels) <- names(x[, col.fac])
  levels.group <- as.vector(x.mnv$xlevels[[1]])
  results.MANOVA <- list(name = "MANOVA with extra matrix information",
                         T = T, W = W, B = B, x.mnv = x.mnv, group = group,
                         levels.group = levels.group,
                         data.name = deparse(substitute(x)),
                         variables = names(df), data = x)
  class(results.MANOVA) <- "OnewayMANOVA"
  return(results.MANOVA)
}
