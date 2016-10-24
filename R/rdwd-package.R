# documentation of package and both Index datasets

#' Download Climate Data from DWD (German Weather Service)
#'
#' Select weather data from the DWD (Deutscher Wetterdienst) with
#' \code{\link{selectDWD}}, which uses \code{\link{fileIndex}}.\cr
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
#' @seealso \code{\link{metaIndex}}, \code{\link{indexDWD}}
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
#' @seealso \code{\link{fileIndex}}, \code{\link{indexDWD}}
#' @keywords datasets
#' @examples
#'
#' data(metaIndex)
#' head(metaIndex)
#'
#' # in functions, you can use head(rdwd:::metaIndex)
#'
data(metaIndex, envir=environment())


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
# save:
save(metaIndex, file="data/metaIndex.rda")
tools::resaveRdaFiles("data/metaIndex.rda")

}

