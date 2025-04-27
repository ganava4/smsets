#' @title Hotelling's \eqn{T^2} test with extra information
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn.
#' Chapman and Hall/CRC.
#'
#' @description
#' An R function which implements Hotelling's \eqn{T^2} test assuming equal
#' covariance matrices, with extra information.
#'
#' @param x a data frame with one two-level factor and \emph{p} response
#' variables.
#' @param group two-level factor defining groups. It must be one of the columns
#' in \code{x}.
#' @param level1 a character string identifying Sample 1. The string must be one
#' of the factor levels in \code{group}.
#'
#' @details
#' This function is a simplified version of the function
#' \code{\link[Hotelling]{hotelling.test}} implemented in the \code{Hotelling}
#' package for the comparison of mean values of two multivariate samples, under
#' the assumption that covariance matrices are equal. The \code{summary} methods
#' in \code{Hotelling.mat} gives more detailed information of the calculations
#' behind the \eqn{T^2} test.
#'
#' @return Returns an object of class \code{"Hotelling.mat"}, a list containing
#' the following components:
#' \tabular{lllll}{
#'    \code{ name} \tab A character string describing the function. \tab   \cr
#'    \code{T2.list} \tab A list containing two data frames with the mean vector
#'    for the two samples, two covariance matrices, one matrix per sample,
#'    the pooled covariance matrix, the inverse of the pooled covariance matrix,
#'    the Hotelling's \eqn{T^2} statistic, the \eqn{F}-statistic, the degrees of
#'    freedom for the \eqn{F}-statistic and the P-value.
#'    \tab  \cr
#'    \code{group} \tab a character string specifying the name of the two-level
#'    factor defining groups. \tab   \cr
#'    \code{levels.group} \tab a vector of length two, showing the two levels in
#'    factor \code{group}. \tab   \cr
#'    \code{data.name} \tab a character string giving the name of the data. \tab
#'      \cr
#'    \code{data} \tab the data frame analyzed. \tab  \cr
#' }
#'
#' @examples
#' data(sparrows)
#' results.T2 <- Hotelling.mat(sparrows, group = Survivorship, level1 = "S")
#' # Brief output
#' results.T2
#'
#' @import data.table
#' @importFrom stats aggregate pf var
#' @export Hotelling.mat
#'
Hotelling.mat <- function(x, group, level1) {
  group <- deparse(substitute(group))
  T2.list <- vector(mode = 'list', length = 10)
  dt <- data.table :: as.data.table(x)
  result <- dt[, var(.SD), by = group]
  result <- as.matrix(result$V1)
  fac <- as.factor(x[, names(x) %in% c(group)])
  fac <- droplevels(fac)
  levels.group <- c(level1, levels(fac)[levels(fac) != level1])
  df <- x[,!names(x) %in% c(group)]
  p <- df.numF <- ncol(df)
  vmeans <- aggregate(df, list(Group = fac), FUN = "mean")
  rownames(vmeans) <- levels(fac)
  names.matcv <- c(level1, levels(fac)[levels(fac) != level1])
  CVs <- array(result, dim = c(p, p, 2),
               dimnames = list(names(df), names(df), names.matcv))
  T2.list[[1]] <- vmeans[vmeans$Group == level1, -1]
  T2.list[[2]] <- CVs[, , level1]
  T2.list[[3]] <- vmeans[vmeans$Group != level1, -1]
  T2.list[[4]] <- CVs[, , levels(fac)[levels(fac) != level1]]
  n1 <- length(fac[fac == level1])
  n2 <- length(fac[fac != level1])
  C1 <- T2.list[[2]]
  C2 <- T2.list[[4]]
  denom <- n1 + n2 - 2
  C <- ((n1 - 1)*C1 + (n2 - 1)*C2)/denom
  T2.list[[5]] <- C
  T2.list[[6]] <- solve(C)
  x1bar <- t(T2.list[[1]])
  x2bar <- t(T2.list[[3]])
  inv.C <- T2.list[[6]]
  T2 <- as.vector(n1 * n2 * t(x1bar - x2bar) %*% inv.C %*%
                    (x1bar - x2bar)/(n1 + n2))
  T2.list[[7]] <- T2
  df.denF <- n1 + n2 - p - 1
  F.stat <- as.vector(df.denF * T2/((n1 + n2 - 2) * p))
  df.F <- c(df.numF, df.denF)
  T2.list[[8]] <- F.stat
  T2.list[[9]] <- df.F
  p.value <- pf(F.stat, df.numF, df.denF, lower.tail = FALSE)
  T2.list[[10]] <- p.value
  names(T2.list) <- c(paste("Mean vector:"),
                      paste("Covariance matrix, C1:", level1),
                      paste("Mean vector:"),
                      paste("Covariance matrix, C2:",
                            levels(fac)[levels(fac) != level1]),
                      "Pooled covariance matrix, C",
                      "Inverse of C",
                      "Hotelling's T2 statistic",
                      "F statistic",
                      "Degrees of freedom",
                      "P-value")
  results.T2 <- list(name = "Hotelling's T2 test", T2.list = T2.list,
                     group = group, levels.group = levels.group,
                     data.name = deparse(substitute(x)), data = x)
  class(results.T2) <- "Hotelling.mat"
  return(results.T2)
}
