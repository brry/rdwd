#' Files available on the DWD CDC FTP server
#'
#' A data.frame with the filenames with path starting at the default \code{base} value:
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{index2df}} in the not-tested example section of \code{\link{indexDWD}}.
#'
#' @name indexlist
#' @docType data
#' @format data.frame with character srings. 25'631 rows x 7 columns:
#'         \code{res}, \code{var}, \code{time} (see \code{\link{metaDWD}}),
#'         station \code{id},
#'         \code{start} and \code{end} of time series according to \code{path}.
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @keywords datasets
#' @examples
#'
#' data(indexlist)
#' head(indexlist)
#'
NULL




#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{metaDWD}}, \code{\link{indexDWD}} and \code{\link{indexlist}}.\cr
#' Download and process data sets with \code{\link{dataDWD}} and \code{\link{readDWD}}.
#'
#' @details
#' ToDo: more info
#'
#' @name rdwd
#' @aliases rdwd-package rdwd
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2016
#' @seealso
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}
#' @keywords package documentation
#' @examples
#'
#' # ToDo
#'
NULL



