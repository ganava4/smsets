% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VanValen.R
\name{VanValen}
\alias{VanValen}
\title{van Valen's test}
\usage{
VanValen(x, group, level1, alternative = "two.sided", var.equal = FALSE)
}
\arguments{
\item{x}{a data frame with one two-level factor and \emph{p} response
variables.}

\item{group}{two-level factor defining groups. It must be one of the columns
in \code{x}.}

\item{level1}{a character string identifying Sample 1. The string must be one
of the factor levels in \code{group}.}

\item{alternative}{a character string specifying the alternative hypothesis
in the t-test for the comparison of mean pooled distances. Must be one of
\code{"two.sided"} (default), \code{"greater"} or \code{"less"}. You can
specify just the initial letter.}

\item{var.equal}{a logical variable indicating whether to treat the two
variances of pooled distances as being equal. If \code{TRUE} then the pooled
variance is used to estimate the variance; otherwise the Welch
(or Satterthwaite) approximation to the degrees of freedom is used.}
}
\value{
Returns an object of class \code{"VanValen"}, a list containing the
following components:
\tabular{lllllllllllllll}{
\code{ name} \tab A character string describing the function. \cr
\code{std.data} \tab A list with two data frames \code{matlevel1} and
\code{matlevel2} containing the values of the standardized variables for
samples 1 and 2 respectively  \cr
\code{medians.std} \tab A list containing two vectors. The first vector
\code{medians.std1} contains the medians for all standardized variables in
sample 1 as declared in parameter \code{level1}, and the second vector,
\code{medians.std2}, holds the corresponding medians for the other sample.
\cr
\code{dev.median} \tab A list with two data frames \code{dev.median1} and
\code{dev.median2} containing the deviations from sample medians for
samples 1 and 2, respectively. \cr
\code{d.list} \tab A list with two data frames \code{d.level1} and
\code{d.level2} containing the pooled distances of standardized variables
from their corresponding medians for samples 1 and 2, respectively. \cr
\code{means.d} \tab A named numeric vector carrying the mean pooled
distances for samples 1 and 2, respectively \cr
\code{vars.d} \tab A named numeric vector carrying the variance of pooled
distances for samples 1 and 2, respectively \cr
\code{t.vec} \tab A named numeric vector containing the t-statistic, the
degrees of freedom and the p-value for the test, respectively. \cr
\code{alternative} \tab a character string specifying the alternative
hypothesis chosen. \cr
\code{var.equal} \tab A logical variable indicating whether the two
variances were treated as being equal \code{TRUE} or not \code{FALSE}. \cr
\code{group} \tab A character string specifying the name of the two-level
factor defining groups.  \cr
\code{levels.group} \tab A vector of length two, showing the two levels in
factor \code{group}.  \cr
\code{data.name} \tab A character string giving the name of the data.  \cr
\code{variables} \tab A character string vector containing the variable
names.  \cr
\code{data} \tab The data frame analyzed.  \cr
}
}
\description{
Computes van Valen's test for the comparison of the variation in two
multivariate samples. The comparison is made in terms of distances between
all standardized variables from their corresponding standardized medians,
thus producing two sets of pooled distances, one per sample, whose means
are then compared by a two-sample t-test.
}
\details{
To ensure that all variables are given equal weight, each variable is first
standardized in van Valen's test, so that the mean is zero and variance
is one for all samples combined before the calculation of the pooled
distances. These are given by

\deqn{d_{ij} = \sqrt{\sum_{k = 1}^{p}{(x_{ijk}-M_{jk})^2}}}

where

\eqn{x_{ijk}} is the value of the standardized variable \eqn{X_{k}} for the
\eqn{i}th individual in sample \eqn{j}, and

\eqn{M_{jk}} is the median of the same standardized variable in the \eqn{j}th
sample.

The sample means of the \eqn{d_{ij}} values are compared with a t-test. If
one sample is more variable than another, then the mean \eqn{d_{ij}} values
will tend to be higher in that sample. The expression for \eqn{d_{ij}} in van
Valen's is based on an implicit assumption that if the two samples being
tested differ, then one sample will be more variable than the other for all
variables. A significant result cannot be expected in a case where, for
example, \eqn{X_1} and \eqn{X_2} are more variable in sample 1, but \eqn{X_3}
and \eqn{X_4} are more variable in sample 2. The effect of the differing
variances would then tend to cancel out in the calculation of \eqn{d_{ij}}.
Thus, Van Valen's test is not appropriate for situations where changes in
the level of variation are not expected to be consistent for all variables.
}
\examples{
data(sparrows)
res.VanValen <- VanValen(sparrows, "Survivorship", "S",
                         alternative = "less", var.equal = TRUE)
# Brief output
res.VanValen

}
\references{
Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024)
\emph{Multivariate Statistical Methods. A Primer}. 5th Edn. CRC Press.

van Valen, L. (1978) The statistics of variation. \emph{Evolutionary Theory}
4: 33-43. (Erratum \emph{Evolutionary Theory} 4: 202.)
}
\author{
Jorge Navarro Alberto, \email{ganava4@gmail.com}
}
