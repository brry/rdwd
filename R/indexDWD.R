#' Create an index of the DWD CDC FTP Server
#'
#' Create a list of all the files (in subfolders) at the Climate Data Center (CDC)
#' FTP-Server from the German Weather Service (DWD, Deutscher WetterDienst) at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#'
#' @details
#' It's not suggested to run this for all folders, as it can take quite some time
#' and you may get kicked off the FTP-Server. This package contains an index
#' of the 'regular' climatic observations at weather stations.
#' ToDo: put reference here.
#' If it is out of date, please let me know!
#'
#' @return currently a vector with file paths (output may change in the future)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
# @seealso \code{\link{metaDWD}}, \code{\link{dataDWD}}, \code{\link{readDWD}}
#' @keywords file
#' @importFrom stats runif
#' @export
#' @examples
#' \dontrun{ ## Needs internet connection
#' sol <- indexDWD(folder="daily/solar")
#' head(sol)
#' }
#'
#' @param folder Folder to be indexed recursively, e.g. "hourly/wind/".
#'               DEFAULT: "" (all folders at \code{base})
#' @param base Main directory of DWD ftp server, defaulting to observed climatic records.
#'             DEFAULT: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
#' @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#'              is passed to \code{\link{Sys.sleep}} after each folder level
#'              to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#'
indexDWD <- function(
folder="",
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
sleep=0,
dir="DWDdata",
quiet=FALSE
)
{
# Check if RCurl is available:
if(!requireNamespace("RCurl", quietly=TRUE))
  stop("The R package 'RCurl' is not available. indexDWD can not obtain file list.\n",
       "install.packages('RCurl')       to enable this.")
f <- folder
# as long as f contains folders (charstrings without . ), run the following:
isfile <- grepl(pattern=".", x=f, fixed=TRUE)
while(any(!isfile))
  {
  isfile <- grepl(pattern=".", x=f, fixed=TRUE)
  f1 <- f[isfile] # these are finished
  f2 <- lapply(f[!isfile], function(path) # for the folders, run getURL:
    {
    # List of files at 'path':
    p <- RCurl::getURL(paste0(base,"/",path,"/"),
                       verbose=F, ftp.use.epsv=TRUE, dirlistonly=TRUE)
    p <- strsplit(p, "\r\n")[[1]]
    # p <- strsplit(p, "\n")[[1]] # may be needed on linux. toDo: check this out
    # complete file path:
    paste0(path,"/",p)
    })
  f <- c(f1,unlist(f2))
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  }
# sort final results alphabetically
f <- sort(unlist(f, use.names=FALSE))
# write output:
##outfile <- paste0("INDEX_of_DWD_", gsub("/","_",base2),".txt")
##if(file.exists(outfile)) warning("File '",outfile,"' already existed, is now overwritten.")
##write.table(f, file=outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
# return output:
return(f)
}

# Index creation:
if(FALSE){
ind1 <- indexDWD(folder="hourly")
ind2 <- indexDWD("daily")
ind3 <- indexDWD("monthly")
}


