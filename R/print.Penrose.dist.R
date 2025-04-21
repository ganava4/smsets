#' Prints Penrose's distance matrix
#'
#' Prints the results produced by Penrose.dist
#'
#' @param x an object of class Penrose.dist
#' @param long a logical variable indicating whether a long output is desired
#' (\code{TRUE}) or not (\code{FALSE}, the default). In addition to Penrose's
#' distances, the long output displays the covariance matrix for each group with
#' their sample sizes, the mean vector for each group, and the pooled covariance
#' matrix.
#' @param ... further arguments passed to or from other methods.
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
    cat("\nSample sizes\n")
    print(x$Samp.sizes)
    cat("\nMean vectors\n")
    print(round(x$means.vec, 2))
    cat("\nSample covariance matrices\n")
    print(lapply(x$covs.list, round, 2))
    cat("Pooled covariance matrix\n")
    print(round(x$PooledCov, 2))
  }
  cat("\nPenrose distances\n")
  print(round(x$Penrose.dist, 3))
  invisible(x)
}

