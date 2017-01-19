# documentation of package and both Index datasets + viewIndex


# package documentation --------------------------------------------------------

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
#' # see README.md
#'
#' link <- selectDWD("Potsdam", res="monthly", var="kl", per="h", current=TRUE)
#' clim <- dataDWD(link)
#' clim$month <- substr(clim$MESS_DATUM_BEGINN,5,6)
#' temp <- tapply(clim$LUFTTEMPERATUR, clim$month, mean)
#' prec <- tapply(clim$NIEDERSCHLAGSHOEHE, clim$month, mean)
#' library(berryFunctions)
#' climateGraph(temp, prec, main="Potsdam 1893:2015")
#'
NULL



# fileIndex --------------------------------------------------------------------

#' Files available on the DWD CDC FTP server
#'
#' A data.frame with the filenames with path starting at the default \code{base} value:
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{createIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
#' In functions, you can access it with \code{rdwd:::fileIndex}.
#'
#' @name fileIndex
#' @docType data
#' @format data.frame with character srings. 25'631 rows x 7 columns:
#'         \code{res}, \code{var}, \code{per} (see \code{\link{selectDWD}}),
#'         station \code{id} and time series \code{start} and \code{end}
#'         according to \code{path}.
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{metaIndex}}, \code{\link{indexDWD}}, \code{\link{selectDWD}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords datasets
#' @examples
#'
#' data(fileIndex)
#' head(fileIndex)
#'
#' # in functions, you can use head(rdwd:::fileIndex), but I don't export it
#' # because Hadley says 'Never @export a data set' in
#' # browseURL("http://r-pkgs.had.co.nz/data.html#data-data")
#'
data(fileIndex, envir=environment())
# http://stackoverflow.com/questions/32964741/accessing-sysdata-rda-within-package-functions
# http://stackoverflow.com/questions/9521009/how-do-you-handle-r-data-internal-to-a-package



# metaIndex --------------------------------------------------------------------

#' station info (meta data) available on the DWD CDC FTP server
#'
#' A data.frame with the contents of all the station description files
#' (..._Beschreibung_Stationen.txt) in the folders hourly, daily and monthly at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{createIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
#' In functions, you can access it with \code{rdwd:::metaIndex}.
#'
#' @name metaIndex
#' @docType data
#' @format data.frame with ca 38k rows for 12 columns:
#'         \code{Stations_id}, \code{von_datum}, \code{bis_datum}
#'         \code{Stationshoehe}, \code{geoBreite}, \code{geoLaenge}
#'         \code{Stationsname}, \code{Bundesland},
#'         \code{res}, \code{var}, \code{per} (see \code{\link{selectDWD}}),
#'         \code{hasfile}
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{geoIndex}} for metadata per location,
#'          \code{\link{fileIndex}}, \code{\link{findID}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords datasets
#' @examples
#'
#' data(metaIndex)
#' head(metaIndex)
#' # in functions, you can use head(rdwd:::metaIndex)
#'
#' # Map of all precipitation stations:
#' if(FALSE){ ## requires extra package
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
data(metaIndex, envir=environment())




# metaInfo ---------------------------------------------------------------------

#' Information for a station ID on the DWD CDC FTP server
#'
#' @return invisible data.frame. Also \code{\link{print}s} the output.
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
id <- as.integer(id)
# Selection of rows:
sel <- metaIndex$Stations_id==id
if(hasfileonly) sel <- sel & metaIndex$hasfile
# Output preparation:
out <- metaIndex[sel,]
# Print preparation:
printout <- t(cbind(out, data.frame("."="")))
colnames(printout) <- 1:ncol(printout)
print(printout, quote=FALSE)
# Print unique paths:
#cat(sort(unique(paste(out$res, out$var, out$per, sep="/"))), sep="\n")
printout <- unique(data.frame(out$res, out$var, out$per))
colnames(printout) <- c("res","var","per")
printout <- sortDF(printout, "per", decreasing=FALSE)
printout <- sortDF(printout, "var", decreasing=FALSE)
printout <- sortDF(printout, "res", decreasing=FALSE)
rownames(printout) <- NULL
print(printout, quote=FALSE)
# Output:
return(invisible(out))
}



# geoIndex --------------------------------------------------------------------

#' coordinatewise station info (meta data) available on the DWD CDC FTP server
#'
#' \code{\link{metaIndex}} distilled to geographic locations.
#'
#' @name geoIndex
#' @docType data
#' @format data.frame with ca 7k rows for 9 columns:
#'         \code{id}, \code{name}, \code{state}
#'         \code{lat}, \code{long}, \code{ele}
#'         \code{all_elev}, \code{nfiles_coord}, \code{nfiles_id}
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{metaIndex}}, \code{\link{createIndex}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords datasets
#' @examples
#'
#' data(geoIndex)
#' head(geoIndex)
#' # in functions, you can use head(rdwd:::geoIndex)
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
data(geoIndex, envir=environment())



# create / update Indexes ------------------------------------------------------

if(FALSE){
# dwdfiles <- indexDWD(sleep=3) # commented out to prevent accidental calling
# dwdfiles <- indexDWD(dwdfiles, sleep=30) # potentially needed several times if sleep small
# file.rename("DWDdata/INDEX_of_DWD__daily_kl_historical.txt", "DWDdata/INDEX_of_DWD_.txt")
dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt") # 25'631 elements (2016-10-21)
index <- createIndex(dwdfiles, meta=TRUE, sleep=10)
fileIndex <- index[[1]]
metaIndex <- index[[2]]
 geoIndex <- index[[3]]

# save and compress:
save(fileIndex,  file="data/fileIndex.rda")
tools::resaveRdaFiles("data/fileIndex.rda") #devtools::use_data(fileIndex, internal=TRUE)
save(metaIndex,  file="data/metaIndex.rda")
tools::resaveRdaFiles("data/metaIndex.rda")
save( geoIndex,  file="data/geoIndex.rda")
tools::resaveRdaFiles("data/geoIndex.rda")

# check writing and reading of the files:
fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==fileIndex2))
metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all(metaIndex==metaIndex2))
 geoIndex2 <- read.table("DWDdata/geoIndex.txt",  sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all( geoIndex== geoIndex2))



# interactive map --------------------------------------------------------------
#data("geoIndex")
onerow <- function(x) paste0("metaInfo(id=",removeSpace(x[1]),")<br>",
                             paste0(names(x)[-1], ": ", x[-1], collapse="<br>"))
#onerow(geoIndex[1,])
geoIndex$display <- apply(geoIndex, MARGIN=1, onerow)

library(leaflet)
map <- leaflet(data=geoIndex) %>% addTiles() %>%
             addCircles(~long, ~lat, radius=900, stroke=F)%>%
             addCircleMarkers(~long, ~lat, popup=~display, stroke=F)
map
htmlwidgets::saveWidget(map, file="map.html")




# tests ------------------------------------------------------------------------

# check coordinates:
coord_ok <- pbsapply(unique(metaIndex$Stationsname), function(n)
  {
  sel <- metaIndex$Stationsname==n
  lat <- metaIndex$geoBreite[sel]
  lon <- metaIndex$geoLaenge[sel]
  ele <- metaIndex$Stationshoehe[sel]
  d <- 6 # number of digits rounded to
  all(round(lat,d)==round(lat[1],d)  &  round(lon,d)==round(lon[1],d)  & ele==ele[1]  )
})
mean(coord_ok) # 79% is OK, 94.9 % with d=2, 98% with d=1
names(coord_ok[!coord_ok])


# some more checks:
mean(metaIndex$hasfile) # 72% has a file
length(unique(metaIndex$Stations_id)) # 5778 IDs (5660 in geoIndex)
hist(table(metaIndex$Stations_id), breaks=100, col="cadetblue", xlab="number of entries per ID")

checkdupli <- function(a,b, x=metaIndex)
  {
  d <- tapply(x[,a], x[,b], unique)
  list( morethan1=d[sapply(d, length)!=1],   table=table(sapply(d, length)) )
  }

checkdupli("Bundesland", "Stationsname") # $`Holzdorf (Flugplatz)` "Sachsen-Anhalt" "Brandenburg"
checkdupli("Stations_id", "Stationsname") # $Hoerstel 2254 15559
checkdupli("Stationsname", "Stations_id") # 53 with 2

checkdupli("name", "id", geoIndex) # 44 with 2

sum(geoIndex$nfiles_coord) # 25482
hist(geoIndex$nfiles_coord, breaks=100, col="cadetblue", xlab="number of files per location")

}
