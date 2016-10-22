#' Create a recursive index of the DWD CDC FTP Server
#'
#' Create a list of all the files (in subfolders) at the Climate Data Center (CDC)
#' FTP-Server from the German Weather Service (DWD, Deutscher WetterDienst) at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}.\cr
#' The R package \code{RCurl} must be available to do this.
#' If \code{RCurl::\link[RCurl]{getURL}} fails, usually because bot access is
#' detected and denied, there will stil be an output which you can pass in a
#' second run via \code{folder} to extract the remaining dirs.
#' You might want to wait a bit and set \code{sleep} to a higher value in that case.
#'
#' @details
#' It's not suggested to run this for all folders, as it can take quite some time
#' and you may get kicked off the FTP-Server. This package contains an index
#' of the climatic observations at weather stations: \code{View(rdwd:::\link{fileIndex})}
#' If it is out of date, please let me know!
#'
#' @return currently a vector with file paths (output may change in the future)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
# @seealso \code{\link{metaDWD}}, \code{\link{dataDWD}}, \code{\link{readDWD}}
#' @keywords file
#' @importFrom stats runif
#' @importFrom utils write.table
#' @export
#' @examples
#' \dontrun{ ## Needs internet connection
#' sol <- indexDWD(folder="/daily/solar")
#' head(sol)
#' rm(sol)
#'
#'
#' # Here's how I produce and update   ?fileIndex    ---------------------------
#' # index <- indexDWD(sleep=30) # commented out to prevent accidental calling
#' # index <- indexDWD(index, sleep=30) # potentially needed several times
#' index <- readLines("DWDdata/INDEX_of_DWD_.txt") # 25'631 elements (2016-10-21)
#' indexcompare <- index2df(index)
#' fileIndex <- read.table("DWDdata/INDEX.txt", sep="\t", header=TRUE, colClasses="character")
#' stopifnot(all(fileIndex==indexcompare))
#'
#' save(fileIndex, file="data/fileIndex.rda")
#' tools::resaveRdaFiles("data/fileIndex.rda")
#' #devtools::use_data(fileIndex, internal=TRUE)
#'
#'
#' # Here's how I produce and update   ?metaIndex    ----------------------------
#' sel <- substr(fileIndex$path, nchar(fileIndex$path)-3, 1e4)==".txt"
#' sel <- sel & grepl("Beschreibung", fileIndex$path)
#' fileIndex[sel, -(4:6)]
#' base <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
#'
#' # comparison with all folders:
#' sum(sel) #27
#' folders <- with(fileIndex, unique(paste(res,var,time, sep="/"))   )
#' length(folders) # 30 (3 multi_annual, no Beschreibung file)
#' folders;  rm(folders)
#'
#' # ToDo: actually create metalist
#'
#' }
#'
#' @param folder Folder to be indexed recursively, e.g. "/hourly/wind/".
#'               DEFAULT: "" (all folders at \code{base})
#' @param base Main directory of DWD ftp server, defaulting to observed climatic records.
#'             DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param ziponly Logical: return only the filenames ending in ".zip"?
#'                DEFAULT: FALSE
#' @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#'              is passed to \code{\link{Sys.sleep}} after each read folder
#'              to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory and failed
#'              \code{RCurl::\link[RCurl]{getURL}}? DEFAULT: FALSE
#' @param progbar Logical: present a progress bar in each level?
#'                Only works if the R package pbapply is available. DEFAULT: TRUE
#'
indexDWD <- function(
folder="",
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
ziponly=FALSE,
sleep=0,
dir="DWDdata",
quiet=FALSE,
progbar=!quiet
)
{
if(ziponly) stop("ziponly is not implemented yet in indexDWD!") # ToDo
# Check if RCurl is available:
if(!requireNamespace("RCurl", quietly=TRUE))
  stop("The R package 'RCurl' is not available. indexDWD can not obtain file list.\n",
       "install.packages('RCurl')       to enable this.")
# Progress bar?
progbar <- progbar & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
# central object: f (file/folder names)
f <- folder
#
# as long as f contains folders (charstrings without . ), run the following:
isfile <- grepl(pattern=".", x=f, fixed=TRUE)
while(any(!isfile))
  {
  isfile <- grepl(pattern=".", x=f, fixed=TRUE)
  f1 <- f[isfile] # these are finished
  f2 <- lapply(f[!isfile], function(path) # for the folders, run getURL:
    {
    # List of files at 'path':
    p <- try( RCurl::getURL(paste0(base,"/",path,"/"),
                       verbose=F, ftp.use.epsv=TRUE, dirlistonly=TRUE), silent=TRUE)
    if(inherits(p, "try-error"))
      {
      if(!quiet) warning("indexDWD: RCurl::getURL failed for '/", path,
                         "/' - ", p, call.=FALSE) # strsplit(p, "\n")[[1]][2]
      assign("isfile", TRUE, inherits=TRUE) # to get out of the while loop
      return(path)
      }
    p <- strsplit(p, "\r\n")[[1]]
    # p <- strsplit(p, "\n")[[1]] # may be needed on linux. toDo: check this out
    if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
    # complete file path:
    return(paste0(path,"/",p))
    })
  f <- c(f1,unlist(f2))
  }
# sort final results alphabetically:
f <- sort(unlist(f, use.names=FALSE))
# Keep only the zip files:
# ToDo
# write output:
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
outfile <- paste0("INDEX_of_DWD_", gsub("/","_",folder[1]),".txt")
outfile <- fileDWD(outfile, quiet=quiet)
write.table(f, file=outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
# return output:
return(f)
}
