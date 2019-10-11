# Documentation of package, meta-data indexes
# Release questions, code for meta-info related functions, index updating

# rdwd-package
# dwdbase
# release_questions
# fileIndex, metaIndex, geoIndex
# metaInfo
# rowDisplay
# DEU Map dataset
# code to create (and update) indexes



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
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016, June 2017
#' @keywords package documentation
#' @seealso USA data: \href{https://www.rdocumentation.org/packages/countyweather}{countyweather},
#'          \href{https://www.rdocumentation.org/packages/rnoaa}{rnoaa}\cr
#'          World data: \href{https://ropensci.org/blog/blog/2017/04/04/gsodr}{Global Surface Summary of the Day}\cr
#'          Durch data: \url{https://github.com/bvhest/KNMIr}\cr
#'          Canadian data: \url{https://cran.r-project.org/package=rclimateca}\cr
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
#' @description  Reminders when using devtools::release. Code is in R/rdwd-package.R
#' @keywords internal

release_questions <- function() {
  c(
    "Have you updated the indexes with the code in R/rdwd-package.R?"
  )
}



# fileIndex, metaIndex, geoIndex, gridIndex, formatIndex ----

#' Indexes of files and metadata on the DWD CDC FTP server
#' 
#' Created with \code{\link{createIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
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



# metaInfo ---------------------------------------------------------------------

#' Information for a station ID on the DWD CDC FTP server
#' 
#' @return invisible data.frame. Also \code{\link{print}s} the output nicely formatted.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{metaIndex}}
#' @keywords datasets
#' @importFrom berryFunctions sortDF
#' @export
#' @examples
#' metaInfo(2849)
#' 
#' @param id Station ID (integer number or convertible to one)
#' @param hasfileonly Logical: Only show entries that have files? DEFAULT: TRUE
#' 
metaInfo <- function(
  id,
  hasfileonly=TRUE
  )
{
# ID preparation:
id <- as.integer(id[1])
# Selection of rows:
sel <- metaIndex$Stations_id==id
if(sum(sel)<1) stop("rdwd::metaIndex contains no entries for id=", id,
                    ". This ID probably does not exist.")
# public / nonpublic files
nonpubmes <- ""
nonpub <- !metaIndex[sel,"hasfile"]
if(any(nonpub)&hasfileonly) nonpubmes <- paste0("\nAdditionally, there are ",
      sum(nonpub), " non-public files. Display all with  metaInfo(",id,",FALSE)",
      "\nTo request those datasets, please contact cdc.daten@dwd.de or klima.vertrieb@dwd.de")
if(hasfileonly) sel <- sel & metaIndex$hasfile
# Output preparation:
out <- metaIndex[sel,]
out$von_datum <- as.Date(as.character(out$von_datum), "%Y%m%d")
out$bis_datum <- as.Date(as.character(out$bis_datum), "%Y%m%d")
#
# Print preparation I:
p_id <- toString(unique(out$Stations_id))
p_sn <- toString(unique(out$Stationsname))
p_bl <- toString(unique(out$Bundesland))
p_nf <- length(unique(paste(out$res, out$var, out$per)))
if(p_id=="") p_id <- id
# message I:
message("rdwd station id ", p_id, " with ", p_nf, " files.\nName: ", p_sn, ", State: ", p_bl, nonpubmes)
#
if(nrow(out)==0) return()
#
# Print preparation II:
p_out <- data.frame(from=out$von_datum,
                    to=out$bis_datum,
                    lat=out$geoBreite,
                    long=out$geoLaenge,
                    ele=out$Stationshoehe)
p_out <- cbind(out[,c("res","var","per","hasfile")], p_out)
p_out$from <- as.character(p_out$from)
###p_out$from[p_out$per=="recent"] <- ""
p_out <- sortDF(p_out, "var", decreasing=FALSE)
p_out <- sortDF(p_out, "res", decreasing=FALSE)
p_out <- sortDF(p_out, "per", decreasing=FALSE)
rownames(p_out) <- NULL
# print II:
print(p_out, quote=FALSE)
#
# Output:
return(invisible(out))
}



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



# DEU Map dataset --------------------------------------------------------------

#' Map of German states (Bundeslaender) from GADM through the \code{raster} package
#' @name DEU
#' @seealso \code{\link{addBorders}}, \code{\link{EUR}}
#' @details Obtained with the code: \cr
#' \code{DEU1 <- raster::getData("GADM", country="DEU", level=1)}\cr
#' \code{DEU <- rgeos::gSimplify(DEU1, tol=0.02, topologyPreserve=FALSE)}\cr
#' \code{raster::plot(DEU1)}\cr
#' \code{raster::plot(DEU)}\cr
#' \code{save(DEU,        file="data/DEU.rda")}\cr
#' \code{tools::resaveRdaFiles("data/DEU.rda")}\cr
#' @docType data
#' @format Formal class 'SpatialPolygons' [package "sp"] with 4 slots
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2018
#' @keywords datasets
data(DEU, envir=environment())


# EUR Map dataset --------------------------------------------------------------

#' Map of Western European countries through the \code{rworldmap} package
#' @name EUR
#' @seealso \code{\link{addBorders}}, \code{\link{DEU}}
#' @details Obtained with the code: \cr
#' \code{EUR <- rworldmap::getMap("low")}\cr
#' \code{EUR <- raster::crop(EUR, c(-5,20, 40,60)) }\cr
#' \code{raster::plot(EUR)}\cr
#' \code{save(EUR,        file="data/EUR.rda", version=2)}\cr
#' \code{tools::resaveRdaFiles("data/EUR.rda", version=2)}\cr
#' @docType data
#' @format SpatialPolygonsDataFrame [package "sp"] with 32 rows
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @keywords datasets
data(EUR, envir=environment())



# addBorders -------------------------------------------------------------------

#' @title add country and Bundesland borders to a map
#' @return invisible list with DEU and EUR
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso \code{\link{DEU}}, \code{\link{EUR}}
#' @keywords aplot
#' @export
#' @examples
#' plot(1, xlim=c(2,16), ylim=c(47,55)) 
#' addBorders()
#' plot(1, xlim=c(2,16), ylim=c(47,55))
#' addBorders(de="orange", eu=NA)
#'
#' @param de      Color for Bandeslaender line. NA to suppress. DEFAULT: "grey80"
#' @param eu      Color for countries line. NA to suppress. DEFAULT: "black"
#' @param add     Logical: add to existing plot? DEFAULT: TRUE
#' @param \dots   Further arguments passed to \code{raster::\link[raster]{plot}}
addBorders <- function(de="grey80", eu="black", add=TRUE, ...)
{
checkSuggestedPackage("raster", "addBorders")
data("DEU","EUR", envir=environment())
raster::plot(DEU, add=add, border=de, ...) 
raster::plot(EUR, add=TRUE, border=eu, ...)
return(invisible(list(DEU=DEU, EUR=EUR)))
}



# update Indexes ---------------------------------------------------------------

if(FALSE){
dwdfiles <- indexFTP(sleep=0, filename="", overwrite=TRUE)
  # dwdfiles <- indexFTP(dwdfiles, sleep=2, filename="", overwrite=TRUE)
  # potentially needed several times with small sleep values on restrictive FTP servers

grdfiles <- indexFTP("currentgindex",   filename="grids", base=gridbase, overwrite=TRUE)

# delete meta folder for truly new data
# check for duplicate description files (Monatwerte + Monatswerte, e.g., also in INDEX_OF.txt)

dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt")
#  25'757 elements (2017-03-14) 
# 218'593 (2018-03-25)
# 228'830 (2018-11-26)
# 240'737 (2019-02-19)
# 242'584 (2019-03-11)
# 266'860 (2019-05-15)
# 254'446 (2019-05-30)
# 255'252 (2019-07-31)
# 254'925 (2019-09-17)
grdfiles <- readLines("DWDdata/INDEX_of_DWD_grids.txt")
#  49'247 (2019-05-26)
#  49'402 (2019-05-30)
#  54'314 (2019-07-31)
#  56'759 (2019-09-17)

# Change meta files manually:
# - 10_minutes_wind_now_zehn_now_ff_Beschreibung_Stationen.txt
# - 10_minutes_extreme_wind_now_zehn_now_fx_Beschreibung_Stationen.txt
# First data row cannot contain spaces in the station name
# ToDo: maybe improve readDWD.meta for that e.g. with checking spaces in all lines...

index <- createIndex(paths=dwdfiles, meta=TRUE) # ca 200 secs +40 if files are not yet downloaded
cat(index$checks)
{ # save indexes into package:
fileIndex <- index[[1]]
metaIndex <- index[[2]]
 geoIndex <- index[[3]]
gridIndex <- grdfiles
# save and compress:
message("saving index rda files...")
# to enable R versions <3.5.0 (2018-04, only one year old at time of writing)
# version=2 see https://github.com/r-lib/devtools/issues/1912
save(fileIndex, file="data/fileIndex.rda", version=2)
save(metaIndex, file="data/metaIndex.rda", version=2)
save( geoIndex, file="data/geoIndex.rda" , version=2)
save(gridIndex, file="data/gridIndex.rda", version=2)
message("compressing files:")
tools::resaveRdaFiles("data/fileIndex.rda", version=2) #devtools::use_data(fileIndex, internal=TRUE)
cat("1")
tools::resaveRdaFiles("data/metaIndex.rda", version=2)
cat("2")
tools::resaveRdaFiles("data/geoIndex.rda" , version=2)
cat("3")
tools::resaveRdaFiles("data/gridIndex.rda", version=2)
cat("4\n")
message("checking files...")
# check writing and reading of the files:
fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==fileIndex2, na.rm=TRUE)) # NAs in ID for subdaily/multi_annual
metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all(metaIndex==metaIndex2))
 geoIndex2 <- read.table("DWDdata/geoIndex.txt",  sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all( geoIndex== geoIndex2))
rm(fileIndex2,metaIndex2,geoIndex2)
rm(index,dwdfiles)
#
#
# read and save subdaily format description:
message("reading standard_format description file...")
format_url <- paste0(dwdbase,"/subdaily/standard_format/formate_kl.html")
format_html <- readLines(format_url, encoding="UTF-8")
message("saving formatIndex...")
format_html <- gsub("&nbsp;", "", format_html)
format_html <- gsub("&#176;", "degree", format_html)
format_html <- format_html[!grepl("Formatbeschreibung", format_html)]
formatIndex <- XML::readHTMLTable(doc=format_html, header=TRUE, stringsAsFactors=FALSE)[[1]]
formatIndex$dividebyten <- grepl("0.1", formatIndex$Einheit)
formatIndex$Einheit <- gsub("0.1 ", "", formatIndex$Einheit)
save(formatIndex, file="data/formatIndex.rda", version=2)
tools::resaveRdaFiles( "data/formatIndex.rda", version=2)
#
message("Now update dwdparams as well, see misc/developmentNotes.R")
} # end saving+checking index files

} # end if FALSE
