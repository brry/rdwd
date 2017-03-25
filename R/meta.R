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
#' @format data.frame with character strings. ca 25k rows x 7 columns:
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
#' @format data.frame with ca 36k rows for 12 columns:
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
#' @return invisible data.frame. Also \code{\link{print}s} the output nicely formatted.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso \code{\link{metaIndex}}, \code{\link{geoIndex}}
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



# geoIndex --------------------------------------------------------------------

#' coordinatewise station info (meta data) available on the DWD CDC FTP server
#'
#' \code{\link{metaIndex}} distilled to geographic locations.
#' \code{geoIndexAll} contains all coordinates.
#' \code{geoIndex} is an aggregated version where stations (of a single ID) with all unique
#' coordinates less than 900 m apart are aggregated into one location.
#' Distance is computed with \url{https://github.com/brry/OSMscale/blob/master/R/maxEarthDist.R}
#' To reduce package dependency, aggregation is done locally in the last section of
#' \url{https://github.com/brry/rdwd/blob/master/R/meta.R}
#'
#' @name geoIndex
#' @aliases geoIndex geoIndexAll
#' @docType data
#' @format data.frame with ca 7k rows for 9 columns:
#'         \code{id}, \code{name}, \code{state}
#'         \code{lat}, \code{long}, \code{ele}
#'         \code{all_elev}, \code{nfiles_coord}, \code{nfiles_id}
#' @source Deutscher WetterDienst / Climata Data Center  FTP Server
#' @seealso \code{\link{metaIndex}}, \code{\link{createIndex}},
#'         \code{vignette("mapDWD")}, \url{../doc/mapDWD.html}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016 + Feb 2017
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
data(geoIndexAll, envir=environment())



# rowDisplay ---------------------------------------------------------------------

#' create display character string for leaflet map popup from data.frame rows
#'
#' @return Vector of characterstrings, one for each row in x.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2017
#' @seealso \code{\link{geoIndex}}
#' @keywords character
#' @importFrom berryFunctions removeSpace
#' @export
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



# update Indexes ---------------------------------------------------------------

if(FALSE){
# commented out to prevent accidental calling
# dwdfiles <- indexDWD(sleep=0, filename="")
# dwdfiles <- indexDWD(dwdfiles, sleep=1, filename="", overwrite=TRUE)
  # potentially needed several times with small sleep values on restrictive FTP servers

# delete meta folder for truly new data
# check for dupliate description files (Monatwerte + Monatswerte, e.g., also in INDEX_OF.txt)

dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt") # 25'757 elements (2017-03-14)
index <- createIndex(dwdfiles, meta=TRUE)
fileIndex    <- index[[1]]
metaIndex    <- index[[2]]
 geoIndexAll <- index[[3]]
# save and compress:
save(fileIndex,     file="data/fileIndex.rda")
tools::resaveRdaFiles(   "data/fileIndex.rda") #devtools::use_data(fileIndex, internal=TRUE)
save(metaIndex,     file="data/metaIndex.rda")
tools::resaveRdaFiles(   "data/metaIndex.rda")
save( geoIndexAll,  file="data/geoIndexAll.rda")
tools::resaveRdaFiles(   "data/geoIndexAll.rda")

# check writing and reading of the files:
fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==fileIndex2))
metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all(metaIndex==metaIndex2))
 geoIndexAll2 <- read.table("DWDdata/geoIndexAll.txt",  sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all( geoIndexAll== geoIndexAll2))
rm(fileIndex2,metaIndex2,geoIndexAll2)
rm(index,dwdfiles)



# geoIndexAll 2 geoIndex -------------------------------------------------------
#    data("geoIndexAll")

# compute max distances:
id <- unique(geoIndexAll$id)
dist <- pbapply::pbsapply(id, function(i)  # ca 5 secs computing time
  {
  g <- geoIndexAll[geoIndexAll$id==i,]
  if(nrow(g)<2) return(0)
  OSMscale::maxEarthDist(lat, long, data=g)
  }) ; names(dist) <- id

# Examine distances:
if(FALSE){
logHist(dist, breaks=50, main="Max distance between station locations in km")
library(leaflet)

farapart <- geoIndexAll[geoIndexAll$id %in% id[dist>0.5],]
farapart$dist <- dist[as.character(farapart$id)]
farapart$display <- paste0(farapart$display, "<br>maxDist: ", round(farapart$dist,2))
col <- seqPal(100)[classify(farapart$dist, method="logspaced", breaks=c(100,1.05))$index]
#col_leg <- seqPal(100)[classify(1:26/2, method="logspaced", breaks=c(100,1.05),
#Range=range(farapart$dist))$index]
mapfarapart <- leaflet(farapart) %>% addTiles() %>%
   addCircleMarkers(~long,~lat, popup=~display, col="white", opacity=1,
                    fillOpacity=1, fillColor=col) #%>%
#   addLegend("bottomright", values=1:26/2, col=col_leg, labels=1:26/2)
htmlwidgets::saveWidget(mapfarapart, "mapfarapart.html")
rm(mapfarapart, col, farapart)
}

# combine stations per ID if closer than 900 m apart (radius of fixed circles):
geoIndex <- pbapply::pblapply(id, function(i){
  g <- geoIndexAll[geoIndexAll$id==i,]
  if(nrow(g)<2) return(g)
  if(dist[as.character(i)] > 0.9) return(g)
  nf_co <- strsplit(paste(g$nfiles_coord,"(0"), "(", fixed=TRUE)
  nf_id <- strsplit(paste(g$nfiles_id,   "(0"), "(", fixed=TRUE)
  nfc <- as.numeric(sapply(nf_co, "[", 1))
  nfi <- as.numeric(sapply(nf_id,    "[", 1))
  nf_co <- sapply(nf_co, "[", 2)
  nf_id <- sapply(nf_id, "[", 2)
  nf_co <- gsub("+","",gsub(")","",nf_co,fixed=TRUE), fixed=TRUE)
  nf_id <- gsub("+","",gsub(")","",nf_id,fixed=TRUE), fixed=TRUE)
  nfc <- nfc + as.numeric(nf_co)
  nfi <- nfi + as.numeric(nf_id)
  g$recentfile <- any(g$recentfile)
  g$ele <- round(sum(g$ele*nfc/nfi[1],na.rm=TRUE),2)
  g$nfiles_coord <- paste(g$nfiles_coord, collapse=" + ")
  return(g[which.max(nfc),])
})

geoIndex <- do.call(rbind, geoIndex)
geoIndex$all_elev <- NULL
geoIndex$display <- NULL
geoIndex$display <- rowDisplay(geoIndex)
geoIndex$col <- "blue"
geoIndex$col[!geoIndex$recentfile] <- "red"
isnul <- as.numeric(sapply(strsplit(geoIndex$nfiles_id, "(", fixed=TRUE), "[", 1))==0
geoIndex$col[isnul] <- "black" ;  rm(isnul)

save( geoIndex,  file="data/geoIndex.rda")
tools::resaveRdaFiles("data/geoIndex.rda")

}
