#' Egyptian male skulls
#'
#' Measurements made on male skulls from the area of Thebes in Egypt. There are
#' samples of 30 skulls from each of five periods: the Early Predynastic period
#' (circa 4000 BC), the Late Predynastic period (circa 3300 BC), the 12th and
#' 13th Dynasties (circa 1850 BC), the Ptolemaic period (circa 200 BC), and the
#' Roman period (circa AD 150). Four measurements (mm) are available on each skull.
#'
#' @docType data
#'
#' @usage data(skulls)
#'
#' @format A data frame with 150 rows and 5 variables:
#' \describe{
#'   \item{\code{Period}}{A factor with five levels}
#'   \item{\code{Maximum_breadth}}{a numeric vector}
#'   \item{\code{Basibregmatic_height}}{a numeric vector}
#'   \item{\code{Basialveolar_length}}{a numeric vector}
#'   \item{\code{Nasal_height}}{a numeric vector}
#'}
#'
#' @keywords datasets
#'
#' @references
#' Thomson, A. and Randall-Maciver, P. (1905). \emph{Ancient Races of the
#' Thebaid}, Oxford University Press, Oxford, London.
#'
#' Manly, B.F.J., Navarro Alberto, J.A. and Gerow, K. (2024) \emph{Multivariate
#' Statistical Methods. A Primer}. 5th Edition. Boca Raton, CRC Press.
#'
#' @examples
#' data(skulls)
#' str(skulls)
"skulls"
