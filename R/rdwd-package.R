# package documentation and release questions

#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with  \code{\link{selectDWD}}. \cr
#' Download and process data sets with \code{\link{dataDWD}} and \code{\link{readDWD}}.\cr
#' Station selection is done offline with \code{\link{fileIndex}} and
#' \code{\link{findID}} (which uses \code{\link{metaIndex}}).\cr
#' The Index objects are created with \code{\link{indexDWD}} and \code{\link{createIndex}}.\cr
#' \code{\link{dirDWD}} and \code{\link{fileDWD}} make sure no file is overwritten and give useful messages.\cr
#' For an introduction to the package, see the vignette \url{../doc/rdwd.html}.
#'
#' @details The following folders in \bold{\code{res/var/per}} notation
#' (resolution/variable/period) are available at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}
#' (and a few more at the \code{res} level).\cr
#' "<" signifies a split into the folders \code{per} = "recent" and "historical".\cr
#' "-" signifies that there are no further sub-folders.
#' \tabular{lll}{
#' \code{res}=\bold{hourly} \tab | \code{res}=\bold{daily} \tab | \code{res}=\bold{monthly} \cr
#' \code{var=} \tab \tab \cr
#'                    \tab | kl <               \tab | kl <          \cr
#'                    \tab | more_precip <      \tab | more_precip < \cr
#' air_temperature <  \tab |                    \tab |               \cr
#' cloudiness <       \tab |                    \tab |               \cr
#' precipitation <    \tab |                    \tab |               \cr
#' pressure <         \tab |                    \tab |               \cr
#' sun <              \tab |                    \tab |               \cr
#' wind <             \tab |                    \tab |               \cr
#' soil_temperature < \tab | soil_temperature < \tab |               \cr
#' solar -            \tab | solar -            \tab |               \cr
#' }
#'
#' @name rdwd
#' @aliases rdwd-package rdwd
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords package documentation
#' @seealso USA data: \url{https://www.rdocumentation.org/packages/countyweather},
#'          \url{https://www.rdocumentation.org/packages/rnoaa}\cr
#'
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
