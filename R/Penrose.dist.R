#' @title Penrose's distance calculator
#'
#' @author Jorge Navarro Alberto, \email{ganava4@@gmail.com}
#'
#' @references
#'
#' da Silva, A.R. (2021). \emph{biotools: Tools for Biometry and Applied
#' Statistics in Agricultural Science}. R package version 4.2.
#' https://cran.r-project.org/package=biotools.
#'
#' da Silva, A.R., Malafaia, G., and Menezes, I.P.P. (2017). biotools: an R
#' function to predict spatial gene diversity via an individual-based approach.
#' \emph{Genetics and Molecular Research} 16.
#' https://doi.org/10.4238/gmr16029655.
#'
#' Goslee, S.C. and Urban, D.L. (2007). The ecodist package for
#' dissimilarity-based analysis of ecological data. \emph{Journal of Statistical
#' Software} 22(7):1-19. DOI:10.18637/jss.v022.i07
#'
#' Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
#' \emph{Multivariate Statistical Methods. A Primer}. 5th Edn.
#' Chapman and Hall/CRC.
#'
#' Penrose, L.W. (1953). Distance, size and shape. \emph{Annals of Eugenics} 18:
#' 337-43.
#'
#' @description
#' Computes Penrose's distance between \emph{m} multivariate populations
#' or samples, when information is available on the means and variances.
#'
#' @param x A data frame with \eqn{p + 1} columns (one factor and \emph{p}
#' response variables).
#' @param group The classification factor defining \emph{m} samples or groups.
#' It must be one of the variables in \code{x}.
#'
#' @details
#' Let the mean of \eqn{X_k} in population \emph{i} be \eqn{\mu_{ki}},
#' \eqn{k=1,...,p; i=1,...,m} and assume that the variance of variable \eqn{X_k}
#' is \eqn{V_k}. The Penrose (1953) distance \eqn{P_{ij}} between population
#' \emph{i} and population \emph{j} is given by
#'
#' \deqn{P_{ij} = \sum_{k = 1}^{p} \frac{(\mu_{ki} - \mu_{kj})^2}{pV_k}}
#'
#' Penrose's distances between multivariate samples are computed using this
#' expression, but \eqn{\mu_{ki}}, \eqn{\mu_{kj}} and \eqn{V_k} being replaced
#' by their corresponding sample estimates.
#'
#' A disadvantage of Penrose's measure is that it does not consider the
#' correlations between the \emph{p} variables.
#'
#' The function requires package \pkg{biotools} (da Silva, 2017, 2021).
#'
#' @return Returns an object of class \code{"Penrose.dist"}, a list containing
#' the following components:
#' \tabular{llllllllllll}{
#'    \code{ name} \tab A character string describing the function. \cr
#'    \code{means.vec} \tab A numeric matrix with \emph{p} rows and \emph{m}
#'    columns giving the mean of each variable per group. \cr
#'    \code{covs.list} \tab A list containing the \emph{m} sample covariance
#'    matrices.  \cr
#'    \code{Samp.sizes} \tab A table showing the number of observations used in
#'    the calculation of the covariance matrix for each group.  \cr
#'    \code{PooledCov} \tab The pooled covariance matrix. This matrix can be
#'    accessed and used as an input argument for the calculation of
#'    Mahalanobis distance in packages \pkg{biotools} (da Silva, 2017, 2021)
#'    and \pkg{ecodist} (Goslee and Urban 2007).
#'    \cr
#'    \code{Penrose.mat} \tab The Penrose distances given as a "\code{matrix}"
#'    object. \cr
#'    \code{Penros.dist} \tab The Penrose distances given as a "\code{dist}"
#'    object. \cr
#'    \code{group} \tab A character string specifying the name of the
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
#' res.Penrose <- Penrose.dist(x = skulls, group = Period)
#' # Brief output
#' res.Penrose
#'
#' @import biotools
#' @importFrom stats aggregate as.dist cov
#' @export Penrose.dist
#'
#
Penrose.dist <- function(x, group)
{
  group <- deparse(substitute(group))
  fac <- x[, names(x) %in% c(group)]
  levels.group <- as.factor(unique(fac))
  df <- x[, !names(x) %in% c(group)]
  col.fac <- which(names(x) == group)
  m <- length(unique(x[, col.fac])) ## Number of groups
  p <- ncol(x) - 1                  ## Number of variables
  n <- nrow(x)                      ## Number of sampling units
  # Creates a list containing the means of each variable, by group
  means.df <- aggregate(df, list(fac), mean)
  # Creates a list: the covariance matrix for all the variables, by group
  covs.list <- by(df, fac, var, simplify = FALSE)
  covs.list
  ngroup.list <- table(fac, dnn = group)  # Number of observations per group
  V.pool <- biotools :: boxM(df, fac)$pooled  # Pooled Covariance matrix
  V.pool
  # Initializing Penrose's distance matrix
  P <- matrix(rep(0,m*m), nrow = m,ncol = m)  # Initializing Penrose's matrix
  for (j in 1:m)
  { for (i in 1:m)
     { if (i==j) {P[i,i]=0 }  else
        { for (k in 1:p) {
    P[i,j] <- P[i,j]+(((means.df[i,(k+1)]-means.df[j,(k+1)])^2)/(p*V.pool[k,k]))
                         }
        }
     }
  }
  P <- P[, levels.group]
  P <- P[levels.group, ]
  colnames(P) <- rownames(P) <- levels.group
  P.Dist <- as.dist(P)                     # Penrose distances
  means.df <- means.df[levels.group, -1]
  rownames(means.df) <- levels.group
  means.vec <- t(means.df)
  covs.list1 <- vector(mode = "list", length = m)
  for (i in 1:m) {
    covs.list1[[i]] <- covs.list[[as.character(levels.group[i])]]
  }
  names(covs.list1) <- levels.group
  ngroup.list <- ngroup.list[levels.group]
  levels.group <- as.character(unique(fac))
  results.Penrose.dist <- list(name = "Penrose distances",
                               means.vec = means.vec,
                               covs.list = covs.list1, Samp.sizes = ngroup.list,
                               PooledCov = V.pool, Penrose.mat = P,
                               Penrose.dist = as.dist(P), group = group,
                               levels.group = levels.group,
                               data.name = deparse(substitute(x)),
                               variables = names(df), data = x)
  class(results.Penrose.dist) <- "Penrose.dist"
  return(results.Penrose.dist)
}
