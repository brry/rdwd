#' Create file and meta index of the DWD CDC FTP Server
#'
#' This is mainly an internal function.
#' Create data.frames out of the vector index returned by \code{\link{indexDWD}}.
#' For \code{\link{fileIndex}} (the first output element) \code{createIndex}
#' tries to obtain res, var, per, file, id, start and end from the paths.
#' If \code{meta=TRUE}, \code{\link{metaIndex}} and \code{\link{geoIndexAll}} are also
#' created. They combine all Beschreibung files into a single data.frame.\cr
#' If you create your own index as suggested in selectDWD (argument \code{findex}),
#' you can read the produced file as shown in the example section.
#'
#' @return invisible data.frame (or if meta=TRUE, list with two data.frames)
#' with a number of columns inferred from the paths. Each is also written to disc.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Nov 2016
#' @seealso \code{\link{indexDWD}}, \code{\link{fileIndex}}, \code{\link{metaIndex}}, \code{\link{selectDWD}}
#' @keywords manip
#' @importFrom berryFunctions l2df convertUmlaut
#' @importFrom utils write.table
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
#' browseURL("https://github.com/brry/rdwd/blob/master/R/meta.R")
#' # where fileIndex and metaIndex are added to the package
#'
#' # Read results in later:
#' \dontrun{ ## files normally not yet available:
#' fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE,
#'                          colClasses="character")
#' metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
#' }
#'
#' @param paths Char: vector of DWD paths returned by \code{\link{indexDWD}} called
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
#'              description files are downloaded to if meta=TRUE. vPassed to
#'              \code{\link{dataDWD}}. "" to write in \code{dir}. DEFAULT: "meta"
#' @param mname Char: Name of file in \code{dir} (not \code{metadir}) in which to
#'              write \code{\link{metaIndex}}.
#'              Use \code{mname=""} to suppress writing. DEFAULT: "metaIndex.txt"
#' @param gname Ditto for \code{\link{geoIndexAll}}. DEFAULT: "geoIndexAll.txt"
#' @param quiet Logical: Suppress messages about directory / filename? DEFAULT: FALSE
#' @param \dots  Further arguments passed to \code{\link{dataDWD}} for the meta part.
#'
createIndex <- function(
paths,
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
dir="DWDdata",
fname="fileIndex.txt",
meta=FALSE,
metadir="meta",
mname="metaIndex.txt",
gname="geoIndexAll.txt",
quiet=FALSE,
...
)
{
# fileIndex --------------------------------------------------------------------
# All paths should have the same amount of levels before being splitted:
fileIndex <- gsub("solar/", "solar//", paths)
fileIndex <- gsub("solar//ignore", "solar/ignore", fileIndex)
fileIndex <- gsub("multi_annual/", "multi_annual//", fileIndex)
fileIndex <- gsub("subdaily/standard_format/", "subdaily/standard_format//", fileIndex)
# remove leading slashes:
fileIndex <- ifelse(substr(fileIndex,1,1)=="/", substr(fileIndex,2,1e4), fileIndex)
# split into parts:
fileIndex <- l2df(lapply(fileIndex,function(x) strsplit(x,"/")[[1]]))
# check if there are actually 4 columns (might be different with non-standard base)
if(ncol(fileIndex)!=4) stop(traceCall(1, "in ", ": "), "index does not have 4 columns, but ",
                            ncol(fileIndex), call.=FALSE)
colnames(fileIndex) <- c("res","var","per","file")
file <- fileIndex$file
fileIndex <- fileIndex[,1:3] # file will be re-attached (with path) as the last column
#
# Get detailed info from file name elements:
info <- l2df(lapply(file, function(x) rev(strsplit(x, "[-_.]")[[1]])))
# Station ID (identification number):
fileIndex$id <- ""
fileIndex$id <- ifelse(fileIndex$per=="historical" & info[,1]=="zip", info[,5], fileIndex$id)
fileIndex$id <- ifelse(fileIndex$per=="recent"     & info[,1]=="zip", info[,3], fileIndex$id)
fileIndex$id <- ifelse(fileIndex$var=="solar"      & info[,1]=="zip", info[,2], fileIndex$id) # var==solar
fileIndex$id <- ifelse(substr(file,1,2)=="kl", substr(file,4,8), fileIndex$id) # res==subdaily
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
#
# Write to disc
owd <- dirDWD(dir, quiet=quiet|fname=="" )
on.exit(setwd(owd), add=TRUE)
if(fname!="")
  {
  outfile <- fileDWD(fname, quiet=quiet)
  write.table(fileIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
# Potential (DEFAULT) output:
if(!isTRUE(meta)) return(invisible(fileIndex))
#
#
# metaIndex --------------------------------------------------------------------
# select Beschreibung_.txt files only:
sel <- substr(fileIndex$path, nchar(fileIndex$path)-3, 1e4)==".txt"
sel <- sel & grepl("Beschreibung", fileIndex$path)
sel <- sel & fileIndex$res %in% c("monthly","daily","hourly")
if(sum(sel)<2) stop(traceCall(1, "in ", ": "),
              "There need to be at least two 'Beschreibung' files. (There is ",
              sum(sel),")", call.=FALSE)
# download and read those files:
metas <- dataDWD(paste0(base,fileIndex[sel, "path"]), dir=metadir, ...)
# filenames <- substr(gsub("/","_",fileIndex[sel, "path"]),2,1e4)
# metas <- readDWD(filenames, dir="DWDdata/meta")
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
    stop(traceCall(1, "in ", ": "), "The file ", fileIndex[sel, "path"][i],
         "\nhas incorrect column names: ", toString(cnames[[i]]),".", call.=FALSE))
#
# merge:
if(!quiet) message("Merging meta files...")
metaIndex <- Reduce(function(...) merge(..., all=T), metas)
if(!quiet) message("Processing meta files...")
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
  outfile <- fileDWD(mname, quiet=quiet)
  write.table(metaIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
#
# geoIndexAll ------------------------------------------------------------------
if(!quiet) message("Creating geoIndexAll...")
geoIndexAll <- metaIndex     # Feb 2017  36'091 rows
geoIndexAll$recentfile <- geoIndexAll$per=="recent" | geoIndexAll$bis_datum >
                                     as.numeric(format(Sys.Date()-365,"%Y%m%d"))
geoIndexAll$recentfile <- geoIndexAll$recentfile & geoIndexAll$hasfile
# unique locations:
geoIndexAll$coord <- paste(geoIndexAll$geoBreite, geoIndexAll$geoLaenge, sep="_")
# id column
geoIndexAll$id <- geoIndexAll$Stations_id
# all station names:
name <- tapply(geoIndexAll$Stationsname, geoIndexAll$coord, unique)
name <- sapply(name, paste, collapse=" _ ")
geoIndexAll$name <- name[geoIndexAll$coord]
rm(name)
# lowercase + english column name
geoIndexAll$state <- geoIndexAll$Bundesland
# coordinate columns
geoIndexAll$lat <- geoIndexAll$geoBreite
geoIndexAll$long <- geoIndexAll$geoLaenge
# average elevation:
geoIndexAll$ele <- round(as.numeric(tapply(geoIndexAll$Stationshoehe,
                        geoIndexAll$coord, mean)[geoIndexAll$coord]), 2)
# all elevation entries:
ele <- tapply(geoIndexAll$Stationshoehe, geoIndexAll$coord, table)
ele <- sapply(ele, function(x) paste0(names(x), "(", x, ")", collapse="_"))
geoIndexAll$all_elev <- ele[geoIndexAll$coord]
rm(ele)
# nuber of files per coordinate set:
nf_p <- table(geoIndexAll$coord[ geoIndexAll$hasfile]) # public files
nf_n <- table(geoIndexAll$coord[!geoIndexAll$hasfile]) # non-public files
nf_out <- paste0(nf_p[geoIndexAll$coord], " (+", nf_n[geoIndexAll$coord], ")")
nf_out <- gsub(" (+NA)", "", nf_out, fixed=TRUE)
geoIndexAll$nfiles_coord <- gsub("NA", "0", nf_out, fixed=TRUE)
# nuber of files per ID:
geoIndexAll$Stations_id <- as.character(geoIndexAll$Stations_id)
nf_p <- table(geoIndexAll$Stations_id[ geoIndexAll$hasfile])
nf_n <- table(geoIndexAll$Stations_id[!geoIndexAll$hasfile])
nf_out <- paste0(nf_p[geoIndexAll$Stations_id], " (+", nf_n[geoIndexAll$Stations_id], ")")
nf_out <- gsub(" (+NA)", "", nf_out, fixed=TRUE)
geoIndexAll$nfiles_id <- gsub("NA", "0", nf_out, fixed=TRUE)
rm(nf_p, nf_n, nf_out)
# recent file?:
recfile <- tapply(geoIndexAll$recentfile, geoIndexAll$coord, any)[geoIndexAll$coord]
geoIndexAll$recentfile <- as.logical(recfile)
# reduction of duplicated rows:
geoIndexAll <- geoIndexAll[!duplicated(geoIndexAll$coord), c(15:23,13)]  #  7'219 rows
# popup display column:
geoIndexAll$display <- rowDisplay(geoIndexAll)
# Write to disc
if(gname!="")
  {
  outfile <- fileDWD(gname, quiet=quiet)
  write.table(geoIndexAll, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
# Output -----------------------------------------------------------------------
return(invisible(list(fileIndex=fileIndex, metaIndex=metaIndex, geoIndexAll=geoIndexAll)))
}
