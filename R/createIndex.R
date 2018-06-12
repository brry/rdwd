#' Create file and meta index of the DWD CDC FTP Server
#' 
#' This is mainly an internal function.
#' Create data.frames out of the vector index returned by \code{\link{indexFTP}}.
#' For \code{\link{fileIndex}} (the first output element) \code{createIndex}
#' tries to obtain res, var, per, file, id, start and end from the paths.
#' If \code{meta=TRUE}, \code{\link{metaIndex}} and \code{\link{geoIndex}} are also
#' created. They combine all Beschreibung files into a single data.frame.\cr
#' If you create your own index as suggested in selectDWD (argument \code{findex}),
#' you can read the produced file as shown in the example section.
#' 
#' @return invisible data.frame (or if meta=TRUE, list with two data.frames)
#' with a number of columns inferred from the paths. Each is also written to disc.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Nov 2016, June 2017
#' @seealso \code{\link{indexFTP}}, \code{\link{fileIndex}}, \code{\link{metaIndex}}, \code{\link{selectDWD}}
#' @keywords manip
#' @importFrom berryFunctions l2df convertUmlaut newFilename sortDF traceCall
#' @importFrom utils write.table
#' @importFrom pbapply pbsapply pblapply
#' @importFrom graphics abline
#' @export
#' @examples
#' \dontrun{ # Not tested with R CMD check because of file writing
#' link <- "/daily/kl/historical/tageswerte_00699_19490101_19580630_hist.zip"
#' ind <- createIndex(link, dir=tempdir())
#' ind
#' link2 <- "/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"
#' link3 <- "/daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt"
#' ind2 <- createIndex(c(link,link2,link3), dir=tempdir(), meta=TRUE)
#' lapply(ind2, head)
#' }
#' 
#' # For real usage, see last part of
#' if(interactive())
#' browseURL("https://github.com/brry/rdwd/blob/master/R/rdwd-package.R")
#' # where the Indexes are added to the package
#' 
#' # Read results in later:
#' \dontrun{ ## files normally not yet available:
#' fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE,
#'                          colClasses="character")
#' metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
#' }
#' 
#' @param paths Char: vector of DWD paths returned by \code{\link{indexFTP}} called
#'              with the same \code{base} value as this function
#' @param base  Main directory of DWD ftp server, defaulting to observed climatic records.
#'              DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param dir   Char: writeable directory name where to save the main output(s).
#'              Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param fname Char: Name of file in \code{dir} in which to write \code{\link{fileIndex}}.
#'              Use \code{fname=""} to suppress writing. DEFAULT: "fileIndex.txt"
#' @param meta  Logical: should metaIndex also be created from fileIndex?
#'              Uses \code{\link{dataDWD}} to download files if not present.
#'              DEFAULT: FALSE
#' @param metadir Char: Directory (subfolder of \code{dir}) where original
#'              description files are downloaded to if meta=TRUE. Passed to
#'              \code{\link{dataDWD}}. "" to write in \code{dir}. DEFAULT: "meta"
#' @param mname Char: Name of file in \code{dir} (not \code{metadir}) in which to
#'              write \code{\link{metaIndex}}.
#'              Use \code{mname=""} to suppress writing. DEFAULT: "metaIndex.txt"
#' @param gname Filename for \code{\link{geoIndex}}. DEFAULT: "geoIndex.txt"
#' @param quiet Logical: Suppress messages about progress and filenames? DEFAULT: FALSE
#' @param \dots Further arguments passed to \code{\link{dataDWD}} for the meta part.
#' 
createIndex <- function(
paths,
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
dir="DWDdata",
fname="fileIndex.txt",
meta=FALSE,
metadir="meta",
mname="metaIndex.txt",
gname="geoIndex.txt",
quiet=FALSE,
...
)
{
# fileIndex --------------------------------------------------------------------
compstart <- Sys.time()
messaget <- function(x) message(x, " (",
          round(difftime(Sys.time(), compstart, units="s")), " secs so far)")
# All paths should have the same amount of levels before being splitted:
fileIndex <- gsub("y/solar/", "y/solar//", paths) # hourly and daily only
#fileIndex <- gsub("solar//ignore", "solar/ignore", fileIndex)
fileIndex <- gsub("multi_annual/", "multi_annual//", fileIndex)
fileIndex <- gsub("subdaily/standard_format/", "subdaily/standard_format//", fileIndex)
# remove leading slashes:
fileIndex <- ifelse(substr(fileIndex,1,1)=="/", substr(fileIndex,2,1e4), fileIndex)
prec1min <- substr(fileIndex,1,33) == "1_minute/precipitation/historical"
prec1min <- substr(fileIndex,1,37) != "1_minute/precipitation/historical/ein" & prec1min
any1min <- any(prec1min)
ncolumns <- 4 + any1min # supposed number of columns: 4, 5 if any prec1min in paths
# split into parts:
if(!quiet) messaget("Splitting filenames...")
fileIndex <- l2df(pbapply::pblapply(fileIndex,function(x) strsplit(x,"/")[[1]]))
# check if there are actually 4/5 columns (might be different with non-standard base)
if(ncol(fileIndex)!=ncolumns) stop(traceCall(1, "in ", ": "), "index does not have ",
                        ncolumns," columns, but ", ncol(fileIndex), call.=FALSE)
if(any1min) fileIndex[prec1min,4] <- fileIndex[prec1min,5]
colnames(fileIndex) <- c("res","var","per","file",if(any1min) "dummyfromyear1minute")
file <- fileIndex$file
fileIndex <- fileIndex[,1:3] # file will be re-attached (with path) as the last column
#
# Get detailed info from file name elements:
if(!quiet) messaget("Extracting metadata from filenames...")
info <- l2df(pbapply::pblapply(file, function(x) rev(strsplit(x, "[-_.]")[[1]])))
# Station ID (identification number):
id <- ""
per <- fileIndex$per
sol <- fileIndex$var=="solar" 
zip <- info[,1]=="zip"
id <- ifelse(zip & per=="historical"       , info[,5], id)
id <- ifelse(zip & per=="recent"           , info[,3], id)
id <- ifelse(zip & per=="now"              , info[,3], id)
id <- ifelse(zip & sol & per!="historical" , info[,3], id) # var==solar
id <- ifelse(zip & per=="meta_data"        , info[,2], id)
id <- ifelse(substr(file,1,2)=="kl", substr(file,4,8), id) # res==subdaily
fileIndex$id <- id
rm(id, per, sol, zip)
#
# start and end of time series (according to file name):
ziphist <- fileIndex$per=="historical"  & info[,1]=="zip"
multi <-  fileIndex$res=="multi_annual" & info[,1]=="txt" & info[,3]!="Stationsliste"
# actual selection:
fileIndex$start <- ""
fileIndex$start <- ifelse(ziphist|multi, info[,4], fileIndex$start)
# Analogous for end:
fileIndex$end <- ""
fileIndex$end <- ifelse(ziphist|multi, info[,3], fileIndex$end)
#
# Append path for accurate file reading later on, e.g. with dataDWD:
fileIndex$path <- paths
rownames(fileIndex) <- NULL
#
# Write to disc
owd <- dirDWD(dir, quiet=quiet|fname=="" )
on.exit(setwd(owd), add=TRUE)
if(fname!="")
  {
  outfile <- newFilename(fname, mid=" ", quiet=quiet)
  write.table(fileIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
# Potential (DEFAULT) output:
if(!isTRUE(meta)) return(invisible(fileIndex))
#
#
# metaIndex --------------------------------------------------------------------
# select Beschreibung_.txt files only:
sel <- grepl('.txt$', fileIndex$path)
sel <- sel & grepl("Beschreibung", fileIndex$path)
sel <- sel & fileIndex$res != "subdaily" # has different columns
#sel <- sel & fileIndex$res %in% c("monthly","daily","hourly")
# manual correction March 2018 for duplicate description files:
descdupli <- basename(paths)=="ein_min_rr_Beschreibung_Stationen.txt" & grepl("/20", dirname(paths))
sel <- sel & !descdupli

if(sum(sel)<2) stop(traceCall(1, "in ", ": "),
              "There need to be at least two 'Beschreibung' files. (There is ",
              sum(sel),")", call.=FALSE)
# download and read those files:
metas <- dataDWD(paste0(base,fileIndex[sel, "path"]), dir=metadir, ...)
for(i in seq_along(metas))
  {
  metas[[i]]$res <- fileIndex[sel, "res"][i]
  metas[[i]]$var <- fileIndex[sel, "var"][i]
  metas[[i]]$per <- fileIndex[sel, "per"][i]
  }
#
# check if all files have the same column names:
cnames <- lapply(metas, colnames)
sapply(2:length(cnames), function(i) if(!all(cnames[[i]] == cnames[[1]]))
    warning(traceCall(1, "in ", ": "), "The file ", fileIndex[sel, "path"][i],
         "\nhas incorrect column names: ", toString(cnames[[i]]),
         "\n instead of \n", toString(cnames[[1]]), call.=FALSE))
#
# merge:
if(!quiet) messaget("Merging meta files...")
metaIndex <- Reduce(function(...) merge(..., all=TRUE), metas)
if(!quiet) messaget("Processing meta files...")
metaIndex$Stationsname <- berryFunctions::convertUmlaut(metaIndex$Stationsname)
metaIndex$Bundesland   <- berryFunctions::convertUmlaut(metaIndex$Bundesland)
#
# remove duplicates (some metafiles currently exist twice, "Monatwerte" and "Monatswerte")
# sum(duplicated(metaIndex[,-3])) # 2'294 (out of 38'516) # Whatever
#
# sort alphabetically:
metaIndex <- berryFunctions::sortDF(metaIndex, "Stationsname", decreasing=FALSE)
#
# add column describing whether each entry has a file
filestatID <- suppressWarnings(as.integer(fileIndex$id))
metaComb <- paste(metaIndex$Stations_id, metaIndex$res, metaIndex$var, metaIndex$per, sep="/")
fileComb <- paste(           filestatID, fileIndex$res, fileIndex$var, fileIndex$per, sep="/")
metaIndex$hasfile <- metaComb  %in% fileComb
#
# Write to disc
if(mname!="")
  {
  outfile <- newFilename(mname, mid=": ", quiet=quiet)
  write.table(metaIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
#
# geoIndex ------------------------------------------------------------------
if(!quiet) messaget("Creating geoIndex...")
geoIndex <- metaIndex     # June 2017  35'428 rows
# lowercase + english column names in desired order
geoIndex$id <- geoIndex$Stations_id
geoIndex$name <- geoIndex$Stationsname
geoIndex$state <- geoIndex$Bundesland
geoIndex$lat <- geoIndex$geoBreite
geoIndex$lon <- geoIndex$geoLaenge
# remove old column names
geoIndex$Stations_id <- NULL
geoIndex$Stationsname <- NULL
geoIndex$Bundesland <- NULL
geoIndex$geoBreite <- NULL
geoIndex$geoLaenge <- NULL
#
id_char <- as.character(geoIndex$id)
#
# average elevation per station ID:
#table(tapply(geoIndex$ele, geoIndex$id, function(x) round(diff(range(x)),2) ))
# only up to 0.5m diff
ele <- round(tapply(geoIndex$Stationshoehe, geoIndex$id, mean), 0.1)
geoIndex$ele <- as.numeric(ele[id_char])
rm(ele)
geoIndex$Stationshoehe <- NULL
#
# nuber of public / nonpublic files per station ID:
geoIndex$nfiles    <- table(geoIndex$id[ geoIndex$hasfile])[id_char]
geoIndex$nonpublic <- table(geoIndex$id[!geoIndex$hasfile])[id_char]
geoIndex$nfiles   [is.na(geoIndex$nfiles   )] <- 0
geoIndex$nonpublic[is.na(geoIndex$nonpublic)] <- 0
#
# recent file?:
recentfile <- geoIndex$per=="recent" | geoIndex$bis_datum >
                                     as.numeric(format(Sys.Date()-365,"%Y%m%d"))
recentfile <- recentfile & geoIndex$hasfile
recentfile <- tapply(recentfile, geoIndex$id, any)[id_char]
geoIndex$recentfile <- as.logical(recentfile)
rm(recentfile)
#
# remove columns:
geoIndex$von_datum <- NULL
geoIndex$bis_datum <- NULL
geoIndex$res <- NULL
geoIndex$var <- NULL
geoIndex$per <- NULL
geoIndex$hasfile <- NULL
#
# reduction into unique stations:
geoIndex <- geoIndex[!duplicated(geoIndex), ]  #  ca 6k rows (=unique station IDs)
#
#
# Duplication checks:
dupli <- list(name=paste("rdwd::createIndex coordinate checks", Sys.time()))
coord <- paste(geoIndex$lon, geoIndex$lat, sep="_")
# stations at the same locations:
dupli_c <- duplicated(coord) | duplicated(coord, fromLast=TRUE)
if(any(dupli_c))
  {
  warning("There are ", sum(dupli_c)/2, " sets of coordinates used for more than ",
          "one station id.\nTo see them, type    .Last.value$dupli")
  dupli$ids_at_one_loc <- sortDF(geoIndex[dupli_c, ], "lon", decreasing=FALSE)
  }
# several locations for one station ID:
dupli_c <- tapply(coord, geoIndex$id, function(x)length(unique(x)) ) > 1
if(any(dupli_c))
  {
  warning("There are ", sum(dupli_c), " stations with more than one set of coordinates.",
          "\nTo see them, type    .Last.value$dupli")
  dupli$locs_at_one_id <- geoIndex[dupli_c, ]
  }
#
#
# column for interactive map popup display:
geoIndex$display <- rowDisplay(geoIndex)
# colors for map:
geoIndex$col <- "blue"
geoIndex$col[!geoIndex$recentfile] <- "red"
rownames(geoIndex) <- NULL
#
# Write to disc:
if(gname!="")
  {
  outfile <- newFilename(gname, mid=": ", quiet=quiet)
  write.table(geoIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
#
# Output -----------------------------------------------------------------------
if(!quiet) messaget("Done.")
return(invisible(list(fileIndex=fileIndex, metaIndex=metaIndex, geoIndex=geoIndex, dupli=dupli)))
}





# +++ Old code to get geoIndex from geoIndexAll ----
# DWD updated coordinates in historical metadata June 2017, so now geoIndex==geoIndexAll
if(FALSE){

# compute max distances (for geoIndex):
if(!quiet) messaget("Computing distances between coordinates per station ID...")
id <- unique(geoIndexAll$id)
dist <- pbapply::pbsapply(id, function(i)  # ca 5 secs computing time
  {
  g <- geoIndexAll[geoIndexAll$id==i,]
  if(nrow(g)<2) return(0)
  maxlldist("lat", "long", data=g, each=FALSE)
  }) ; names(dist) <- id
dist <- round(dist, 3)
geoIndexAll$maxdist <- dist[as.character(geoIndexAll$id)]
# Write to disc:
if(aname!="")
  {
  outfile <- newFilename(aname, mid=": ", quiet=quiet)
  write.table(geoIndexAll, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
#
# geoIndex ---
if(!quiet) messaget("Creating geoIndex...")
# combine stations per ID if closer than 900 m apart (radius of fixed circles in vignette map):
geoIndexAll$coord_merged <- FALSE
geoIndex <- pbapply::pblapply(id, function(i){
  g <- geoIndexAll[geoIndexAll$id==i,]
  if(nrow(g)<2) return(g)
  if(dist[as.character(i)] > 0.9) return(g)
  # number of files per coordinate and id:
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
  g$coord_merged <- TRUE
  return(g[which.max(nfc),])
})
geoIndexAll$coord_merged <- NULL
#
# convert to data.frame, remove some columns:
geoIndex <- do.call(rbind, geoIndex)
geoIndex$all_elev <- NULL
geoIndex$display <- NULL
geoIndex$display <- rowDisplay(geoIndex)
isnul <- as.numeric(sapply(strsplit(geoIndex$nfiles_id, "(", fixed=TRUE), "[", 1))==0
geoIndex$col[isnul] <- "black" ;  rm(isnul)


# interactive map of large differences:
#data(geoIndexAll)
logHist(geoIndexAll$maxdist, breaks=50, main="Max distance between station locations in km")
abline(v=c(0.5,0.9))
library(leaflet)

farapart <- geoIndexAll[geoIndexAll$maxdist>0.5,]
farapart$display <- paste0(farapart$display, "<br>maxDist: ", round(farapart$maxdist,2))
col <- seqPal(100)[classify(farapart$maxdist, method="logspaced", breaks=c(100,1.05))$index]
#col_leg <- seqPal(100)[classify(1:26/2, method="logspaced", breaks=c(100,1.05),
#Range=range(farapart$maxdist))$index]
mapfarapart <- leaflet(farapart) %>% addTiles() %>%
   addCircleMarkers(~long,~lat, popup=~display, color="white", opacity=1,
                    fillOpacity=1, fillColor=col) #%>%
#   addLegend("bottomright", values=1:26/2, col=col_leg, labels=1:26/2)
htmlwidgets::saveWidget(mapfarapart, "mapfarapart.html")
rm(mapfarapart, col, farapart)
}
