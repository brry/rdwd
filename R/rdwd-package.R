# package documentation and release questions

#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{selectDWD}} or \code{\link{nearbyStations}}. \cr
#' Download and process data sets with \code{\link{dataDWD}} and \code{\link{readDWD}}.\cr
#' Station selection is done offline with \code{\link{fileIndex}} and
#' \code{\link{findID}} (which uses \code{\link{metaIndex}}).\cr
#' The Index objects are created with \code{\link{indexDWD}} and \code{\link{createIndex}}.\cr
#' For an introduction to the package, see the \href{../doc/rdwd.html}{main vignette}.
#'
#' @name rdwd
#' @aliases rdwd-package rdwd
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords package documentation
#' @seealso USA data: \url{https://www.rdocumentation.org/packages/countyweather},
#'          \url{https://www.rdocumentation.org/packages/rnoaa}\cr
#' @section Searchability Terms:
#' Weather Data Germany download with R, Climate Data Germany\cr
#' Deutscher Wetterdienst R Daten download Klimastationen\cr
#' DWD Daten mit R runterladen, Wetter und Klimadaten in R\cr
NULL



#' Reminders when using devtools::release
#'
#' Reminders when using devtools::release. Code is in R/rdwd-package.R
#'
#' @keywords internal

release_questions <- function() {
  c(
    "Have you updated the indexes with the code in R/meta.R?"
  )
}
