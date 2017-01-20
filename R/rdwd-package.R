# package documentation and general examples

#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{selectDWD}}. Download and process data sets with
#' \code{\link{dataDWD}} and \code{\link{readDWD}}.\cr
#' Station selection is done offline with \code{\link{fileIndex}} and
#' \code{\link{findID}} (which uses \code{\link{metaIndex}}).
#' The Index objects are created with \code{\link{indexDWD}} and \code{\link{createIndex}}.\cr
#' \code{\link{dirDWD}} and \code{\link{fileDWD}} make sure no file is overwritten and give useful messages.\cr
#' For an introduction to the package, see \url{https://github.com/brry/rdwd#rdwd}
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
#'          \url{https://www.rdocumentation.org/packages/rnoaa}
#' @examples
#'
#' \dontrun{ # excluded from CRAN R CMD check because of file writing / time consumption
#'
# # basics -----
#'
#' link <- selectDWD("Potsdam", res="monthly", var="kl", per="h", current=TRUE)
#' clim <- dataDWD(link[1])
#' clim$month <- substr(clim$MESS_DATUM_BEGINN,5,6)
#' temp <- tapply(clim$LUFTTEMPERATUR, clim$month, mean)
#' prec <- tapply(clim$NIEDERSCHLAGSHOEHE, clim$month, mean)
#' library(berryFunctions)
#' climateGraph(temp, prec, main="Potsdam 1893:2015")
#'
# # map metaIndex -----
#'
#' # Map of all precipitation stations:
#' if(!requireNameSpace("OSMscale")) install.packages("OSMscale")
#' library("OSMscale")
#' map <- pointsMap(geoBreite, geoLaenge, data=metaIndex, fx=0.28, fy=0.06)
#' pdf("DWDdata/RainfallStationsMap.pdf")
#' plot(map)
#' scaleBar(map, x=0.05, y=0.03, abslen=200)
#' pp <- projectPoints(geoBreite, geoLaenge, data=metaIndex, to=posm())
#' points(pp[!metaIndex$hasfile,], col="red", pch=3)
#' points(pp[ metaIndex$hasfile,], col="blue", pch=3)
#' legend("bottomright", c("in matadata only", "file on FTP server"),
#'        col=c("red", "blue"), pch=3, bg="white")
#' title(main="DWD stations: data on ftp server", line=3)
#' dev.off()
#'
#'
# # map geoIndex -----
#'
#' # Map of all precipitation stations:
#' if(FALSE){ ## requires extra package
#' if(!requireNameSpace("OSMscale")) install.packages("OSMscale")
#' library("OSMscale")
#' map <- pointsMap(lat, long, data=geoIndex, fx=0.06, fy=0.06)
#' pdf("DWDdata/RainfallStationsMap_nfiles.pdf", width=5)
#' plot(map)
#' scaleBar(map, x=0.05, y=0.03, abslen=200)
#' geoIndex <- sortDF(geoIndex, "nfiles_coord", decreasing=FALSE)
#' pp <- projectPoints(lat, long, data=geoIndex, to=posm())
#' points(pp, cex=0.6)
#' colPoints(pp$x, pp$y, geoIndex$nfiles_coord, cex=0.6, zlab="")
#' title(main="DWD stations: number of files on ftp server", line=3)
#' dev.off()
#' }
#'
# # ts duration -----
#'
#' # Time series duration:
#' colPoints <- berryFunctions::colPoints
#' colPoints(geoLaenge, geoBreite, Stations_id, data=metaIndex, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, Stationshoehe, data=metaIndex, add=F, asp=1.5)
#' metaIndex$von_jahr <- metaIndex$von_datum/1e4
#' metaIndex$bis_jahr <- metaIndex$bis_datum/1e4
#' metaIndex$dauer <- metaIndex$bis_jahr - metaIndex$von_jahr
#' colPoints(geoLaenge, geoBreite, von_jahr, data=metaIndex, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, bis_jahr, data=metaIndex, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, dauer, data=metaIndex, add=F, asp=1.5)
#' hist(metaIndex$bis_jahr, breaks=50, col="purple")
#' hist(metaIndex$dauer, breaks=50, col="purple")
#' sum(metaIndex$dauer>50); mean(metaIndex$dauer>50)
#' # 40% of stations with more than 50 years of data (according to metadata)
#' }
#'
NULL

