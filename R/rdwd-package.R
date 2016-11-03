# documentation of package and both Index datasets + viewIndex

#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{selectDWD}}, which uses \code{\link{fileIndex}} and
#' \code{\link{findID}} (which uses \code{\link{metaIndex}}).
#' The Index objects are created with \code{\link{indexDWD}} and \code{\link{index2df}}.\cr
#' Download and process data sets with \code{\link{dataDWD}} and \code{\link{readDWD}}.
#' code{\link{dirDWD}} and code{\link{fileDWD}} make sure no file is overwritten and give useful messages.\cr
#' For an introduction to the package, see \url{https://github.com/brry/rdwd#rdwd}
#'
#' @details The following folders are available (and a few more at the \code{res} level) at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}\cr
#' "<" signifies a split into the folders \code{time} = "recent" and "historical".\cr
#' "-" signifies that there are no further sub-folders.
#' \tabular{lll}{
#' \code{res}=\bold{hourly} \tab | \code{res}=\bold{daily} \tab | \code{res}=\bold{monthly} \cr
#' \code{var=} \tab \tab \cr
#' air_temperature <  \tab | kl <               \tab | kl <          \cr
#' cloudiness <       \tab | more_precip <      \tab | more_precip < \cr
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
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Oct 2016
#' @keywords package documentation
#' @examples
#'
#' # see README.md
#'
#' link <- selectDWD("Potsdam", res="monthly", var="kl", time="h", current=TRUE)
#' clim <- dataDWD(link)
#' clim$month <- substr(clim$MESS_DATUM_BEGINN,5,6)
#' temp <- tapply(clim$LUFTTEMPERATUR, clim$month, mean)
#' prec <- tapply(clim$NIEDERSCHLAGSHOEHE, clim$month, mean)
#' library(berryFunctions)
#' climateGraph(temp, prec, main="Potsdam 1893:2015")
#'
NULL




#' Files available on the DWD CDC FTP server
#'
#' A data.frame with the filenames with path starting at the default \code{base} value:
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{index2df}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
#' In functions, you can access it with \code{rdwd:::fileIndex}.
#'
#' @name fileIndex
#' @docType data
#' @format data.frame with character srings. 25'631 rows x 7 columns:
#'         \code{res}, \code{var}, \code{time} (see \code{\link{selectDWD}}),
#'         station \code{id} and time series \code{start} and \code{end}
#'         according to \code{path}.
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{metaIndex}}, \code{\link{indexDWD}}, \code{\link{selectDWD}}
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




#' station info (meta data) available on the DWD CDC FTP server
#'
#' A data.frame with the contents of all the station description files
#' (..._Beschreibung_Stationen.txt) in the folders hourly, daily and monthly at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}.
#' Created with \code{\link{index2df}} in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/rdwd-package.R}
#' In functions, you can access it with \code{rdwd:::metaIndex}.
#'
#' @name metaIndex
#' @docType data
#' @format data.frame with character srings. 36'254 rows x 11 columns:
#'         \code{Stations_id}, \code{von_datum}, \code{bis_datum}
#'         \code{Stationshoehe}, \code{geoBreite}, \code{geoLaenge}
#'         \code{Stationsname}, \code{Bundesland},
#'         \code{res}, \code{var}, \code{time} (see \code{\link{selectDWD}})
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{fileIndex}}, \code{\link{findID}}
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




#' View fileIndex and metaIndex
#'
#' Open \code{rdwd:::\link{fileIndex}} and \code{rdwd:::\link{metaIndex}} with \code{\link{View}}
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @importFrom utils View
#' @export
viewIndex <- function() {View(fileIndex); View(metaIndex)}





if(FALSE){

# Here's how I produce and update   ?fileIndex    ---------------------------
# index <- indexDWD(sleep=30) # commented out to prevent accidental calling
# index <- indexDWD(index, sleep=30) # potentially needed several times
index <- readLines("DWDdata/INDEX_of_DWD_.txt") # 25'631 elements (2016-10-21)
indexcompare <- index2df(index)
fileIndex <- read.table("DWDdata/INDEX.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==indexcompare))

save(fileIndex, file="data/fileIndex.rda")
tools::resaveRdaFiles("data/fileIndex.rda")
#devtools::use_data(fileIndex, internal=TRUE)


# Here's how I produce and update   ?metaIndex    ---------------------------
fileIndex <- read.table("DWDdata/INDEX.txt", sep="\t", header=TRUE, colClasses="character")
sel <- substr(fileIndex$path, nchar(fileIndex$path)-3, 1e4)==".txt"
sel <- sel & grepl("Beschreibung", fileIndex$path)
sel <- sel & fileIndex$res %in% c("monthly","daily","hourly")
fileIndex[sel, -(4:6)]
base <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
#metas <- dataDWD(paste0(base,fileIndex[sel, "path"]))
rm(base)
metas <- readDWD(substr(gsub("/","_",fileIndex[sel, "path"]),2,1e4))
for(i in seq_along(metas))
{
  metas[[i]]$res <- fileIndex[sel,  "res"][i]
  metas[[i]]$var <- fileIndex[sel,  "var"][i]
  metas[[i]]$time<- fileIndex[sel, "time"][i]
}
rm(i)

# check if all files have the same column names:
cnames <- sapply(metas, colnames)
stopifnot(all(sapply(2:26, function(i) all(cnames[,i] == cnames[,1]))))
rm(cnames)

# merge:
metaIndex <- Reduce(function(...) merge(..., all=T), metas)

# ASCII symbols:
convertUmlaut <- function(x)
  {
  x <- gsub("\U00FC","ue",gsub("\U00F6","oe",gsub("\U00E4","ae",gsub("\U00DF","ss",x))))
  x <- gsub("\U00DC","Ue",gsub("\U00D6","Oe",gsub("\U00C4","Ae",x)))
  x }
metaIndex$Stationsname <- convertUmlaut(metaIndex$Stationsname)
metaIndex$Bundesland   <- convertUmlaut(metaIndex$Bundesland)
# iconv(metaIndex$Stationsname, to="ASCII//TRANSLIT")

# sort alphabetically:
metaIndex <- berryFunctions::sortDF(metaIndex, Stationsname, decreasing=FALSE)

# add column describing whether each entry has a file
metaComb <- paste(  metaIndex$Stations_id,  metaIndex$res, metaIndex$var, metaIndex$time, sep="/")
fileComb <- paste(as.integer(fileIndex$id), fileIndex$res, fileIndex$var, fileIndex$time, sep="/")
metaIndex$hasfile <- metaComb  %in% fileComb

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
mean(coord_ok) # 80.7% is OK, 94.9 % with d=2, 98% with d=1
names(coord_ok[!coord_ok])

# save and compress:
save(metaIndex, file="data/metaIndex.rda")
tools::resaveRdaFiles("data/metaIndex.rda")

}

