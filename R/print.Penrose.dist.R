#' Prints Penrose's distance matrix
#'
#' Prints the results produced by `Penrose.dist`, the Penrose's distance
#' calculator.
#'
#' @param x an object of class Penrose.dist
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default). In addition to Penrose's
#' distances, the long output displays the covariance matrix for each group with
#' their population / sample sizes, the mean vector for each group, and the
#' pooled covariance matrix.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns
#' Displays Penrose's distances between _m_ multivariate populations or samples.
#' The argument `x`, invisibly, as for all print methods, is a list of class
#' "\code{Penrose.dist}". This `print` method provides two sorts of output
#' depending on whether the `long` argument is `TRUE` or `FALSE` (the default).
#' The "short" output displays:
#' \itemize{
#'   \item A heading describing the function.
#'   \item The data frame analyzed.
#'   \item The variables involved in the calculation of distances.
#'   \item The factor defining the populations or samples and their levels.
#'   \item The Penrose distance matrix (lower triangular form).
#' }
#' In addition to the above information, the "long" output lists:
#' \itemize{
#'    \item The population or sample sizes.
#'    \item The mean vector for each population / sample.
#'    \item The covariance matrix for each population / sample
#'    \item The pooled covariance matrix.
#'    }
#'
#' @examples
#' data(skulls)
#' res.Penrose <- Penrose.dist(x = skulls, group = Period)
#' # Long output
#' print(res.Penrose, long = TRUE)
#'
#' @exportS3Method print Penrose.dist
print.Penrose.dist <- function(x, long = FALSE, ...)
{
  stopifnot(inherits(x, "Penrose.dist"))
  cat("                     Calculation of Penrose distances\n")
  #
  cat("\n Data:", x$data.name, "\n")
  cat(" Variables:", x$variables, "\n")
  cat(" Factor:", x$group, "\n")
  cat(" Levels:", x$levels.group, "\n")
  if (long == TRUE) {
    cat("\nPopulation/Sample sizes\n")
    print(x$Samp.sizes)
    cat("\nMean vectors\n")
    print(round(x$means.vec, 2))
    cat("\nCovariance matrices\n")
    print(lapply(x$covs.list, round, 2))
    cat("Pooled covariance matrix\n")
    print(round(x$PooledCov, 2))
  }
  cat("\nPenrose distances\n")
  print(round(x$Penrose.dist, 3))
  invisible(x)
}

