# Documentation of package and indexes
# Release questions
# small helper functions

# rdwd-package
# dwdbase
# release_questions
# fileIndex, metaIndex, geoIndex
# rowDisplay
# checkSuggestedPackage



# rdwd-package ----

#' Handle Climate Data from DWD (German Weather Service)
#' 
#' - find, select, download + read data from the German weather service DWD\cr
#' - vectorized, progress bars, no re-downloads\cr
#' - index of files + meta data\cr
#' - observational time series from 6k meteorological recording stations (2.5k active)\cr
#'   -> rain, temperature, wind, sunshine, pressure, cloudiness, humidity, snow, ...\cr
#' - gridded raster data from radar + interpolation\cr
#' - european data stock slowly growing\cr
#' For an introduction to the package, see \url{https://bookdown.org/brry/rdwd}.\cr
#' 
#' @name rdwd
#' @aliases rdwd-package rdwd
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @keywords package documentation
#' @seealso USA data: \href{https://www.rdocumentation.org/packages/countyweather}{countyweather},
#'          \href{https://www.rdocumentation.org/packages/rnoaa}{rnoaa}\cr
#'          World data: \href{https://ropensci.org/blog/blog/2017/04/04/gsodr}{Global Surface Summary of the Day}\cr
#'          Durch data: \url{https://github.com/bvhest/KNMIr}\cr
#'          Canadian data: \url{https://cran.r-project.org/package=weathercan}\cr
#' @section Searchability Terms:
#' Weather Data Germany download with R, Climate Data Germany\cr
#' Deutscher Wetterdienst R Daten download Klimastationen\cr
#' DWD Daten mit R runterladen, Wetter und Klimadaten in R
NULL



# dwdbase ----

#' @title DWD FTP Server base URL
#' @aliases gridbase
#' @export
#' @description base URL to DWD FTP Server\cr\cr
#' \code{dwdbase}: observed climatic records at\cr
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate}\cr\cr
#' \code{gridbase}: spatially interpolated gridded data at\cr
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/grids_germany}
#' 
dwdbase <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate"
#' @export
gridbase <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany"


# release_questions ----

#' @title Reminders when using devtools::release
#' @description  Reminders when using devtools::release.
#' @keywords internal

release_questions <- function() {
  c(
    "Have you run updateIndexes() and runLocalTests()?"
  )
}



# fileIndex, metaIndex, geoIndex, gridIndex, formatIndex ----

#' Indexes of files and metadata on the DWD CDC FTP server
#' 
#' Created with \code{\link{indexFTP}} and \code{\link{createIndex}} used in \code{\link{updateIndexes}}.\cr
#' In functions, you can access them with \code{rdwd:::fileIndex} etc.\cr
#' \bold{fileIndex}: A data.frame with the filenames (and derived information)
#' at the default \code{base} value \code{\link{dwdbase}}.\cr
#' \bold{metaIndex}: A data.frame with the contents of all the station description files
#' (..._Beschreibung_Stationen.txt) under \code{\link{dwdbase}}.\cr
#' \bold{geoIndex}: \code{metaIndex} distilled to geographic locations.\cr
#' \bold{gridIndex}: Vector of file paths at \code{\link{gridbase}}.\cr
#' \bold{formatIndex}: (modified) table from 
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/subdaily/standard_format/formate_kl.html}\cr
#' 
#' @name index
#' @aliases fileIndex metaIndex geoIndex gridIndex formatIndex
#' @docType data
#' @format
#' \bold{fileIndex}: data.frame with character strings. ca 260k rows x 8 columns:\cr
#'         \code{res}, \code{var}, \code{per} (see \code{\link{selectDWD}}),
#'         station \code{id}, time series \code{start} and \code{end}, and
#'         \code{ismeta} information, all according to \code{path}.\cr
#' \bold{metaIndex}: data.frame with ca 97k rows for 12 columns:\cr
#'         \code{Stations_id, von_datum, bis_datum,
#'         Stationshoehe, geoBreite, geoLaenge, Stationsname, Bundesland,
#'         res, var, per, hasfile} \cr
#' \bold{geoIndex}: data.frame with ca 6k rows for 11 columns:\cr
#'         \code{id, name, state, lat, lon, ele, nfiles, nonpublic, recentfile, display, col}\cr
#' \bold{gridIndex}: Vector with ca 50k file paths at \code{\link{gridbase}}\cr
#' \bold{formatIndex}: data.frame with 140 rows for 12 columns:\cr
#'         \code{Ke_Ind, Kennung, Label, Beschreibung, Einheit, Code-Tabellen,
#'         Zusatzinfo, Typ, Pos, Erlaubt, Fehlk, dividebyten}\cr
#' @source Deutscher WetterDienst / Climate Data Center  FTP Server
#' @seealso \code{\link{createIndex}}, \code{\link{indexFTP}}, \code{\link{selectDWD}},
#'          \code{\link{findID}}, \code{\link{metaInfo}},
#'          \url{https://bookdown.org/brry/rdwd}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016, June 2017, Oct 2019
#' @keywords datasets
#' @importFrom utils data
#' @examples
#' data(fileIndex)
#' data(metaIndex)
#' data(geoIndex)
#' head(fileIndex)
#' head(metaIndex)
#' head(geoIndex)
#' 
#' # in functions, you can use head(rdwd:::fileIndex) etc, but I don't export them
#' # because Hadley says 'Never @export a data set' in
#' # browseURL("http://r-pkgs.had.co.nz/data.html#data-data")
#' 
data(fileIndex, envir=environment())
data(metaIndex, envir=environment())
data( geoIndex, envir=environment())
data(gridIndex, envir=environment())
data(formatIndex, envir=environment())
# http://stackoverflow.com/questions/32964741/accessing-sysdata-rda-within-package-functions
# http://stackoverflow.com/questions/9521009/how-do-you-handle-r-data-internal-to-a-package



# rowDisplay ---------------------------------------------------------------------

#' Create leaflet map popup from data.frame rows
#' 
#' Create display character string for leaflet map popup from data.frame rows.
#' This function is not exported, as it is only internally useful.
#' A generic version is available in \code{berryFunctions::\link[berryFunctions]{popleaf}}.
#' 
#' @return Vector of character strings, one for each row in x.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2017
#' @seealso \code{\link{geoIndex}}
#' @keywords character
#' @importFrom berryFunctions removeSpace
#' 
#' @param x data.frame with colnames
#' 
rowDisplay <- function(
x
)
{
perrow <- function(x) paste0("rdwd::metaInfo(id=",removeSpace(x[1]),")<br>",
                             paste0(names(x)[-1], ": ", x[-1], collapse="<br>"))
apply(x, MARGIN=1, perrow)
}



# checkSuggestedPackage --------------------------------------------------------

#' @title check suggested package for availability
#' @description check suggested package for availability, 
#'              yielding an instructive error message if not
#' @return invisible success logical value from \code{\link{requireNamespace}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso \code{\link{requireNamespace}}
#' @keywords package
# @export # only needed internally, I think
#'
#' @param package      Charstring: package to be checked for loadability
#' @param functionname Charstring: function name to be used in the message
#'
checkSuggestedPackage <- function(package, functionname)
{
available <- requireNamespace(package, quietly=TRUE)
if(!available) stop("To use ",functionname, ", please first install ",
                    package,":    install.packages('",package,"')", call.=FALSE)
return(invisible(available))
}

