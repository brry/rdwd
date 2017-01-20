# documentation of meta data indexes and functions
# fileIndex
# metaIndex
# metaInfo
# geoIndex
# mapDWD
# code to create (and update) indexes


# fileIndex --------------------------------------------------------------------

#' Files available on the DWD CDC FTP server
#'
#' A data.frame with the filenames with path starting at the default \code{base} value:
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{createIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/meta.R}
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
#' \url{https://github.com/brry/rdwd/blob/master/R/meta.R}
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
#'          \code{\link{fileIndex}}, \code{\link{findID}}, \code{\link{metaInfo}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016
#' @keywords datasets
#' @examples
#'
#' data(metaIndex)
#' head(metaIndex)
#' # in functions, you can use head(rdwd:::metaIndex)
#'
#' # example usages are in ?rdwd
#'
#'
data(metaIndex, envir=environment())



# metaInfo ---------------------------------------------------------------------

#' Information for a station ID on the DWD CDC FTP server
#'
#' @return invisible data.frame. Also \code{\link{print}s} the output.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{metaIndex}}, \code{\link{mapDWD}}
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
#' # example usages are in ?rdwd
#'
data(geoIndex, envir=environment())



# mapDWD --------------------------------------------------------------

#' Interactive map of data available on the DWD CDC FTP server
#'
#' Interactive leaflet map with DWD weather stations.
#' Created from \code{\link{geoIndex}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/meta.R}
#'
#' @name mapDWD
#' @docType data
#' @format leaflet map
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{geoIndex}}, \code{\link{metaInfo}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2017
#' @keywords datasets
#' @examples
#'
#' data(mapDWD)
#' library(leaflet)
#' mapDWD
#' # in functions, you can use rdwd:::mapDWD
#'
#' # The first line when clicked on a point can be copied to R for more information
#'
data(mapDWD, envir=environment())



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


# interactive map:
onerow <- function(x) paste0("metaInfo(id=",removeSpace(x[1]),")<br>",
                             paste0(names(x)[-1], ": ", x[-1], collapse="<br>"))
#onerow(geoIndex[1,])
geoIndex$display <- apply(geoIndex, MARGIN=1, onerow)

library(leaflet)
mapDWD <- leaflet(data=geoIndex) %>% addTiles() %>%
             addCircles(~long, ~lat, radius=900, stroke=F)%>%
             addCircleMarkers(~long, ~lat, popup=~display, stroke=F)
mapDWD
htmlwidgets::saveWidget(mapDWD, file="map.html")
save(mapDWD,     file="data/mapDWD.rda")
tools::resaveRdaFiles("data/mapDWD.rda")
}
