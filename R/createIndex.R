#' Create file and meta index of the DWD CDC FTP Server
#' 
#' This is mainly an internal function.
#' Create data.frames out of the vector index returned by [indexFTP()].
#' For [`fileIndex`] (the first output element) `createIndex`
#' tries to obtain res, var, per, file, id, start and end from the paths.
#' If `meta=TRUE`, [`metaIndex`] and [`geoIndex`] are also
#' created. They combine all Beschreibung files into a single data.frame.\cr
#' If you create your own index as suggested in selectDWD (argument `findex`),
#' you can read the produced file as shown in the example section.
#' 
#' @return invisible data.frame (or if meta=TRUE, list with two data.frames)
#' with a number of columns inferred from the paths. Each is also written to disc.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Nov 2016, June 2017
#' @seealso [indexFTP()], [updateIndexes()], [`index`], [selectDWD()],
#'          [website index chapter](https://bookdown.org/brry/rdwd/fileindex.html)
#' @keywords manip
#' @importFrom berryFunctions l2df convertUmlaut newFilename sortDF tstop twarning seqPal
#' @importFrom utils write.table
#' @importFrom pbapply pbsapply pblapply
#' @importFrom graphics abline
#' @export
#' @examples
#' \dontrun{ # Not tested with R CMD check because of file writing
#' link <- "daily/kl/historical/tageswerte_00699_19490101_19580630_hist.zip"
#' ind <- createIndex(link, dir=tempdir())
#' ind
#' link2 <- "daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"
#' link3 <- "daily/kl/recent/KL_Tageswerte_Beschreibung_Stationen.txt"
#' ind2 <- createIndex(c(link,link2,link3), dir=tempdir(), meta=TRUE, checkwarn=FALSE)
#' lapply(ind2, head)
#' link4 <- "1_minute/precipitation/meta_data/Meta_Daten_ein_min_rr_00755.zip"
#' ind <- createIndex(link4, dir=tempdir())
#' ind
#' }
#' 
#' @param paths Char: vector of DWD paths returned by [indexFTP()] called
#'              with the same `base` value as this function
#' @param base  Main directory of DWD ftp server, defaulting to observed climatic records.
#'              DEFAULT: [`dwdbase`]
#' @param dir   Char: writeable directory name where to save the main output(s).
#'              Created if not existent. DEFAULT: "DWDdata" at current [getwd()]
#' @param fname Char: Name of file in `dir` in which to write [`fileIndex`].
#'              Use `fname=""` to suppress writing. DEFAULT: "fileIndex.txt"
#' @param meta  Logical: should metaIndex also be created from fileIndex?
#'              Uses [dataDWD()] to download files if not present.
#'              DEFAULT: FALSE
#' @param metadir Char: Directory (subfolder of `dir`) where original
#'              description files are downloaded to if meta=TRUE. Passed to
#'              [dataDWD()]. "" to write in `dir`. DEFAULT: "meta"
#' @param mname Char: Name of file in `dir` (not `metadir`) in which to
#'              write [`metaIndex`].
#'              Use `mname=""` to suppress writing. DEFAULT: "metaIndex.txt"
#' @param gname Filename for [`geoIndex`]. DEFAULT: "geoIndex.txt"
#' @param overwrite Logical: Overwrite existing `fname / mname / gname` files?
#'              If not, "_n" is added to the filenames, see
#'              [berryFunctions::newFilename()].
#'              DEFAULT: FALSE
#' @param checkwarn Logical: warn about [checkIndex()] issues? DEFAULT: TRUE
#' @param checklog Logfile for [checkIndex()]. DEFAULT: [tempfile()]
#' @param quiet Logical: Suppress messages about progress and filenames?
#'              DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots Further arguments passed to [dataDWD()] for the meta part.
#' 
createIndex <- function(
paths,
base=dwdbase,
dir="DWDdata",
fname="fileIndex.txt",
meta=FALSE,
metadir="meta",
mname="metaIndex.txt",
gname="geoIndex.txt",
overwrite=FALSE,
checkwarn=TRUE,
checklog=tempfile(),
quiet=rdwdquiet(),
...
)
{
# fileIndex --------------------------------------------------------------------
compstart <- Sys.time()
messaget <- function(x) message(x, " (",
          round(difftime(Sys.time(), compstart, units="s")), " secs so far)")
# overview of all index variants:
if(FALSE){ # for development
    dd <- unlist(pblapply(unique(dirname(paths)), function(p){
     f <- paths[startsWith(paths,paste0(p,"/"))]
     f <- f[tools::file_ext(f)!="pdf"]
     f <- f[tools::file_ext(f)!="html"]
     f <- f[!grepl("Beschreibung",f, fixed=TRUE)]
     f <- f[!duplicated(tools::file_ext(f))]
     f
     }))
    sc <- stringr::str_count(dd, "/")
    dd <- dd[c(which(sc==2), which(sc==4), which(sc==3))]
    dd <- dd[-which(startsWith(dd,"1_minute"))[-1]]
    dd <- dd[-which(startsWith(dd,"5_minute"))[-1]]
    clipr::write_clip(dd)
    # https://docs.google.com/spreadsheets/d/1qXQ1bSLW5TJnJgpUXIID3mVNYS6YZaHbsoe22LmBIAk/edit#gid=0
}
# All paths should have the same amount of levels before being splitted:
err <- "1_minute/precipitation/historical/2021/1minutenwerte_nieder1minutenwerte_nieder_000_hist.zip"
paths <- paths[paths!=err] ; rm(err)
fileIndex <- paths
s <- function(pat, rep) sub(pat, rep, fileIndex, fixed=TRUE)
fileIndex <- s("y/solar/", "y/solar//") # only hourly + daily, not the others
fileIndex <- s("multi_annual/", "multi_annual//")
fileIndex <- s("subdaily/standard_format/", "subdaily/standard_format//")
fileIndex <- s( "1_minute/precipitation/historical/", "1_minute/precipitation/historical|")
fileIndex <- s("5_minutes/precipitation/historical/","5_minutes/precipitation/historical|")
fileIndex <- s("climate_indices/","climate_indices|")
rm(s)
 # remove leading slashes:
fileIndex <- sub("^/","",fileIndex)
# split into parts:
if(!quiet) messaget("Splitting filenames...")
fileIndex <- strsplit(fileIndex,"/", fixed=TRUE)
# check if there are actually 4 columns:
if(any(lengths(fileIndex)!=4)) tstop("index should have 4 columns, not ", ncol(fileIndex))
fileIndex <- data.frame(t(simplify2array(fileIndex))) # much faster than l2df
colnames(fileIndex) <- c("res","var","per","file")
file <- fileIndex$file
fileIndex <- fileIndex[,1:3] # 'path' will be re-attached as the last column
fileIndex$per[startsWith(fileIndex$per, "historical|")] <- "historical"
fileIndex$var[startsWith(fileIndex$var, "climate_indices|")] <- "climate_indices"
#
# Get detailed info from file name elements:
fileIndex$id    <- "" # Station ID (identification number)
fileIndex$start <- ""
fileIndex$end   <- ""
if(!quiet) messaget("Extracting station IDs + time range from filenames...")
now <- grepl("akt.zip",file,fixed=TRUE) | grepl("now.zip",file,fixed=TRUE) | 
       grepl("row.zip",file,fixed=TRUE)
# /CDC/derived_germany/soil/daily/historical/derived_germany_soil_daily_historical_1001.txt.gz
deriv <- grepl("derived_germany",file,fixed=TRUE)
selmeta <- which(tools::file_ext(file) == "zip" & fileIndex$per!="meta_data" & !now & !deriv)
filesel <- file[selmeta]
filesel <- sub("wetter_tageswerte_RR", "wetter_tageswerte|RR", filesel, fixed=TRUE)
filesel <- sub("extrema_temp", "extrema|temp", filesel, fixed=TRUE)
filesel <- sub("extrema_wind", "extrema|wind", filesel, fixed=TRUE)
filesel <- strsplit(filesel, "_", fixed=TRUE)
# info <- l2df(filesel) ; View(info[grepl("\\D",info$V5),])
fileIndex$id   [selmeta] <- sapply(filesel, "[", 3)
fileIndex$start[selmeta] <- sapply(filesel, "[", 4)
fileIndex$end  [selmeta] <- sapply(filesel, "[", 5)
selmeta <- fileIndex$per=="meta_data"
fileIndex$id[selmeta] <-sub(".*_(\\d*)\\.zip"       , "\\1", basename(file[selmeta]))
fileIndex$id[now] <-    sub(".*_(\\d*)_\\D{3}\\.zip", "\\1", basename(file[now])    )
fileIndex$id[deriv] <-  sub(".*_(\\d*)\\.txt\\.gz"  , "\\1", basename(file[deriv])  )
rm(selmeta, deriv, now, file, filesel)
fileIndex$id <- suppressWarnings(as.integer(fileIndex$id))
fileIndex$start <- as.Date(fileIndex$start, "%Y%m%d")
fileIndex$end <- as.Date(fileIndex$end, "%Y%m%d")
#
# standard_format hist/recent
sf <- fileIndex$var=="standard_format" & fileIndex$per==""
fileIndex[sf & grepl("akt.txt", paths, fixed=TRUE), "per"] <- "recent"
fileIndex[sf & grepl("_bis_"  , paths, fixed=TRUE), "per"] <- "historical"
rm(sf)
#
if(!quiet) messaget("Determining if files contain meta data...")
# is the file a metafile?
ma <- fileIndex$res=="multi_annual"
ismeta1 <- !ma & endsWith(paths,'.txt') & grepl("Beschreibung", paths, fixed=TRUE)
ismeta2 <-  ma & grepl("Stationsliste", paths, fixed=TRUE)
ismeta3 <- grepl("meta_data/Meta_Daten", paths, fixed=TRUE)
ismeta4 <- endsWith(paths,'.pdf')  | endsWith(paths,'.html')
fileIndex$ismeta <- ismeta1 | ismeta2 | ismeta3 | ismeta4
rm(ma, ismeta1, ismeta2, ismeta3, ismeta4)
#
# Append original paths for accurate file reading later on, e.g. with dataDWD:
fileIndex$path <- paths
rownames(fileIndex) <- NULL
#
# Write to disc
owd <- dirDWD(dir, quiet=quiet|fname=="" )
on.exit(setwd(owd), add=TRUE)
if(fname!="")
  {
  outfile <- berryFunctions::newFilename(fname, mid=" ", quiet=quiet, ignore=overwrite)
  write.table(fileIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
# Potential (DEFAULT) output:
if(!isTRUE(meta)) return(invisible(fileIndex))
#
#
# metaIndex --------------------------------------------------------------------
if(!quiet) messaget("Generating metaIndex...")
# select Beschreibung_.txt files only:
sel <- endsWith(fileIndex$path, '.txt')
sel <- sel & grepl("Beschreibung_Stationen", fileIndex$path, fixed=TRUE)
if(sum(sel)<2) tstop("There need to be at least two 'Beschreibung' files. (There is ",sum(sel),")")
# download and read those files:
metas <- dataDWD(fileIndex[sel, "path"], base=base, joinbf=TRUE, dir=metadir,
                 overwrite=overwrite, read=FALSE, ...)
metas <- readDWD(metas, type="meta", quiet=TRUE)
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
    twarning("The file ", fileIndex[sel, "path"][i],
         "\nhas incorrect column names: ", toString(cnames[[i]]),
         "\n instead of \n", toString(cnames[[1]]), skip=2))
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
# convert date columns to date:
metaIndex$von_datum <- as.Date(as.character(metaIndex$von_datum),"%Y%m%d")
metaIndex$bis_datum <- as.Date(as.character(metaIndex$bis_datum),"%Y%m%d")
# hist files end date too recent: not my fault, is in DWD Beschreibung files

# Write to disc
if(mname!="")
  {
  outfile <- berryFunctions::newFilename(mname, mid=": ", quiet=quiet, ignore=overwrite)
  write.table(metaIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
#
# geoIndex ------------------------------------------------------------------
if(!quiet) messaget("Creating geoIndex...")
geoIndex <- metaIndex     # June 2017  35'428 rows, 76'772 in Nov 2018
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
#table(tapply(geoIndex$Stationshoehe, geoIndex$id, function(x) round(diff(range(x)),2) ))
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
geoIndex$nfiles    <- as.numeric(geoIndex$nfiles)
geoIndex$nonpublic <- as.numeric(geoIndex$nonpublic)

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
# column for interactive map popup display:
geoIndex$display <- rowDisplay(geoIndex)
geoIndex$display <- gsub("nfiles", "n public files", geoIndex$display, fixed=TRUE)
# colors for map:
geoIndex$col <- "blue"
geoIndex$col[!geoIndex$recentfile] <- "red"
rownames(geoIndex) <- NULL
#
# Write to disc:
if(gname!="")
  {
  outfile <- berryFunctions::newFilename(gname, mid=": ", quiet=quiet, ignore=overwrite)
  write.table(geoIndex, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
  }
#
# Check all indexes:
checks <- checkIndex(fileIndex, metaIndex, geoIndex, fast=TRUE, warn=checkwarn,
                     quiet=quiet, logfile=checklog)
#
# Output -----------------------------------------------------------------------
if(!quiet) messaget("Done.")
return(invisible(list(fileIndex=fileIndex, metaIndex=metaIndex, geoIndex=geoIndex,
                      checks=checks)))
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
  outfile <- berryFunctions::newFilename(aname, mid=": ", quiet=quiet)
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
col <- berryFunctions::seqPal(100)[classify(farapart$maxdist, method="logspaced", breaks=c(100,1.05))$index]
#col_leg <- seqPal(100)[classify(1:26/2, method="logspaced", breaks=c(100,1.05),
#Range=range(farapart$maxdist))$index]
mapfarapart <- leaflet(farapart) %>% addTiles() %>%
   addCircleMarkers(~long,~lat, popup=~display, color="white", opacity=1,
                    fillOpacity=1, fillColor=col) #%>%
#   addLegend("bottomright", values=1:26/2, col=col_leg, labels=1:26/2)
htmlwidgets::saveWidget(mapfarapart, "mapfarapart.html")
rm(mapfarapart, col, farapart)
}
