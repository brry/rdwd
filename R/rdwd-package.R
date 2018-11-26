# Documentation of package, meta-data indexes
# Release questions, code for meta-info related functions, index updating

# rdwd-package
# release_questions
# fileIndex, metaIndex, geoIndex
# metaInfo
# rowDisplay
# code to create (and update) indexes



# rdwd-package ----

#' Download Climate Data from DWD (German Weather Service)
#' 
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{selectDWD}} or \code{\link{nearbyStations}}. \cr
#' Download and process data sets with \code{\link{dataDWD}} and \code{\link{readDWD}}.\cr
#' Station selection is done offline with \code{\link{fileIndex}} and
#' \code{\link{findID}} (which uses \code{\link{metaIndex}}).\cr
#' The Index objects are created with \code{\link{indexFTP}} and \code{\link{createIndex}}.\cr
#' For an introduction to the package, see the \href{../doc/rdwd.html}{main vignette}.\cr
#' For an overview of available data, see \code{\link{selectDWD}}.\cr
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



# release_questions ----

#' Reminders when using devtools::release
#' 
#' Reminders when using devtools::release. Code is in R/rdwd-package.R
#' 
#' @keywords internal

release_questions <- function() {
  c(
    "Have you updated the indexes with the code in R/rdwd-package.R?"
  )
}



# fileIndex, metaIndex, geoIndex ----

#' Indexes of files and metadata on the DWD CDC FTP server
#' 
#' Created with \code{\link{createIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
#' In functions, you can access them with \code{rdwd:::fileIndex} etc.\cr
#' \bold{fileIndex}: A data.frame with the filenames at the default \code{base} value
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.\cr
#' \bold{metaIndex}: A data.frame with the contents of all the station description files
#' (..._Beschreibung_Stationen.txt) in the folders hourly, daily and monthly at \code{base}.\cr
#' \bold{geoIndex}: \code{metaIndex} distilled to geographic locations.
#' 
#' @name index
#' @aliases fileIndex metaIndex geoIndex
#' @docType data
#' @format
#' \bold{fileIndex}: data.frame with character strings. ca 219k rows x 7 columns:\cr
#'         \code{res}, \code{var}, \code{per} (see \code{\link{selectDWD}}),
#'         station \code{id} and time series \code{start} and \code{end}
#'         according to \code{path}.\cr
#' \bold{metaIndex}: data.frame with ca 60k rows for 12 columns:\cr
#'         \code{Stations_id, von_datum, bis_datum,
#'         Stationshoehe, geoBreite, geoLaenge, Stationsname, Bundesland,
#'         res, var, per, hasfile} \cr
#' \bold{geoIndex}: data.frame with ca 6k rows for 11 columns:\cr
#'         \code{id, name, state, lat, lon, ele, nfiles, nonpublic, recentfile, display, col}
#' @source Deutscher WetterDienst / Climate Data Center  FTP Server
#' @seealso \code{\link{createIndex}}, \code{\link{indexFTP}}, \code{\link{selectDWD}},
#'          \code{\link{findID}}, \code{\link{metaInfo}},
#'          \href{../doc/mapDWD.html}{\code{vignette("mapDWD")}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016, June 2017
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
data(geoIndex, envir=environment())
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
      "\nTo request those datasets, please contact  klima.vertrieb@dwd.de")
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


# parameter_abbreviations ---------------------------------------------------------------------

#' Parameter abbreviations for data on the DWD CDC FTP server
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso \code{\link{readVars}}, \code{\link{readDWD}}
#' @keywords datasets
#' @export
#' @examples
#' head(parameter_abbreviations)
#' 
parameter_abbreviations <- read.table(header=TRUE, strip.white=TRUE, 
                                      stringsAsFactors=FALSE, text="
Parameter   Kurz
ATMO_STRAHL Atmospaerenstrahlung
ATMO_LBERG	 Atmospaerenstrahlung_Stundensumme
ASH_6       Ausstichsschneehoehe
NM	         Bedeckungsgrad
V_N        	Bedeckungsgrad
V_S1_NS    	Bedeckungsgrad_Schicht1
V_S2_NS    	Bedeckungsgrad_Schicht2
V_S3_NS    	Bedeckungsgrad_Schicht3
V_S4_NS    	Bedeckungsgrad_Schicht4
VPM        	Dampfdruck
V_TE002	    Erdbodentemperatur_002cm
V_TE005	    Erdbodentemperatur_005cm
V_TE010	    Erdbodentemperatur_010cm
V_TE020	    Erdbodentemperatur_020cm
V_TE002M	   Erdbodentemperatur_02cm
V_TE050	    Erdbodentemperatur_050cm
V_TE005M	   Erdbodentemperatur_05cm
V_TE100	    Erdbodentemperatur_100cm
V_TE010M	   Erdbodentemperatur_10cm
V_TE020M	   Erdbodentemperatur_20cm
V_TE050M	   Erdbodentemperatur_50cm
WASH_6	     Gesamtschneewasseraequivalent
FG_STRAHL  	Globalstrahlung
FG_LBERG	   Globalstrahlung_Stundensumme
FD_STRAHL  	Himmelsstrahlung_diffus
FD_LBERG	   Himmelsstrahlung_diffus_Stundensumme
PM	         Luftdruck
P	          Luftdruck_NN
P0          Luftdruck_Stationshoehe
TMK	        Lufttemperatur
TT_TU       Lufttemperatur
TGK	        Lufttemperatur_5cm_min
TXK	        Lufttemperatur_max
TNK 	       Lufttemperatur_min
RSKF	       Niederschlagsform
RSF 	       Niederschlagsform
WRTR	       Niederschlagsform
RSK	        Niederschlagshoehe
RS	         Niederschlagshoehe
R1	         Niederschlagshoehe
RS_IND	     Niederschlagsindikator
UPM	        Relative_Feuchte
RF_TU 	     Relative_Feuchte
SHK_TAG    	Schneehoehe
SH_TAG	     Schneehoehe
WAAS_6	     Schneewasseraequivalent
V_VV  	     Sichtweite
SDK	        Sonnenscheindauer
SD_STRAHL  	Sonnenscheindauer
SD_SO	      Sonnenscheindauer
SD_LBERG    Sonnenscheindauer_Stundensumme
FM          Windgeschwindigkeit
F	          Windgeschwindigkeit
D	          Windrichtung
FX          Windspitze
V_S1_CSA    Wolkenart_Abk_Schicht1
V_S2_CSA    Wolkenart_Abk_Schicht2
V_S3_CSA    Wolkenart_Abk_Schicht3
V_S4_CSA    Wolkenart_Abk_Schicht4
V_S1_CS	    Wolkenart_Schicht1
V_S2_CS	    Wolkenart_Schicht2
V_S3_CS	    Wolkenart_Schicht3
V_S4_CS	    Wolkenart_Schicht4
V_S1_HHS    Wolkenhoehe_Schicht1
V_S2_HHS    Wolkenhoehe_Schicht2
V_S3_HHS    Wolkenhoehe_Schicht3
V_S4_HHS    Wolkenhoehe_Schicht4

")

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



# DEU Map dataset --------------------------------------------------------------

#' Map of German states (Bundeslaender) from GADM through the \code{raster} package
#' @name DEU
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





# update Indexes ---------------------------------------------------------------

if(FALSE){
# FTP indexing commented out to prevent accidental calling:
# dwdfiles <- indexFTP(sleep=0, filename="")
# dwdfiles <- indexFTP(dwdfiles, sleep=1, filename="", overwrite=TRUE)
  # potentially needed several times with small sleep values on restrictive FTP servers

# delete meta folder for truly new data
# check for dupliate description files (Monatwerte + Monatswerte, e.g., also in INDEX_OF.txt)

dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt") # 25'757 elements (2017-03-14) 218'593 (2018-03-25)
index <- createIndex(paths=dwdfiles, meta=TRUE) # ca 70 secs +30 if files are not yet downloaded
{ # save indexes into package:
fileIndex <- index[[1]]
metaIndex <- index[[2]]
 geoIndex <- index[[3]]
# save and compress:
message("saving index rda files...")
save(fileIndex, file="data/fileIndex.rda")
cat(".")
save(metaIndex, file="data/metaIndex.rda")
save( geoIndex, file="data/geoIndex.rda")
message("compressing files...")
tools::resaveRdaFiles("data/fileIndex.rda") #devtools::use_data(fileIndex, internal=TRUE)
tools::resaveRdaFiles("data/metaIndex.rda")
tools::resaveRdaFiles("data/geoIndex.rda")
message("checking files...")
# check writing and reading of the files:
fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==fileIndex2))
metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all(metaIndex==metaIndex2))
 geoIndex2 <- read.table("DWDdata/geoIndex.txt",  sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all( geoIndex== geoIndex2))
rm(fileIndex2,metaIndex2,geoIndex2)
rm(index,dwdfiles)
} # end saving+checking index files

} # end if FALSE
