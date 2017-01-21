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
#'
#' mon <- indexDWD(folder="/monthly/kl", verbose=TRUE)
#' }
#'
#' @param folder Folder to be indexed recursively, e.g. "/hourly/wind/".
#'               DEFAULT: all folders at \code{base} in current \code{\link{fileIndex}}
#' @param base Main directory of DWD ftp server, defaulting to observed climatic records.
#'             DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#'              is passed to \code{\link{Sys.sleep}} after each read folder
#'              to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory and failed
#'              \code{RCurl::\link[RCurl]{getURL}}? DEFAULT: FALSE
#' @param progbar Logical: present a progress bar in each level?
#'                Only works if the R package pbapply is available. DEFAULT: TRUE
#' @param verbose Logical: write a lot of messages from \code{RCurl::\link[RCurl]{getURL}}?
#'                DEFAULT: FALSE (usually, you dont need all the curl information)
#'
indexDWD <- function(
folder=unique(dirname(fileIndex$path)),
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
sleep=0,
dir="DWDdata",
quiet=FALSE,
progbar=!quiet,
verbose=FALSE
)
{
# Check if RCurl is available:
if(!requireNamespace("RCurl", quietly=TRUE))
  stop("The R package 'RCurl' is not available. rdwd::indexDWD can not obtain file list.\n",
       "install.packages('RCurl')       to enable this.")
# Progress bar?
progbar <- progbar & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
# central object: f (file/folder names)
f <- folder
#
# as long as f contains folders (charstrings without . ), run the following:
isfile <- grepl(pattern=".", x=f, fixed=TRUE)
stoppp <- FALSE
while(any(!isfile))
  {
  f1 <- f[isfile] # these are finished
  #
  f2 <- lapply(f[!isfile], function(path) # for the folders, run getURL:
    {
    # List of files at 'path':
    if(stoppp) return(path) # do not attempt if already kicked off the FTP
    p <- try( RCurl::getURL(paste0(base,"/",path,"/"),
                       verbose=verbose, ftp.use.epsv=TRUE, dirlistonly=TRUE), silent=TRUE)
    if(inherits(p, "try-error"))
      {
      if(!quiet) warning("rdwd::indexDWD: RCurl::getURL failed for '", path,
                         "/' - ", p, call.=FALSE, immediate.=TRUE) # strsplit(p, "\n")[[1]][2]
      assign("isfile", 7777, inherits=TRUE) # to get out of the while loop
      # if(grepl("Access denied: 530", p)) # stoppp now always set to true after failure
      assign("stoppp", TRUE, inherits=TRUE) # to get out of the lapply loop
      return(path)
      }
    # carriage return / newline is OS-dependent:
    p <- unlist(strsplit(p,"[\n\r]")) # http://stackoverflow.com/a/40763124/1587132
    p <- p[nchar(p)>0]
    # wait some time:
    if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
    # complete file path:
    return(paste0(path,"/",p))
    }) # end lapply loop
  f <- c(f1,unlist(f2))
  if(!isfile[1]==7777) isfile <- grepl(pattern=".", x=f, fixed=TRUE)
  } # end while loop
# sort final results alphabetically:
f <- sort(unlist(f, use.names=FALSE))
# write output:
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
outfile <- paste0("INDEX_of_DWD_", gsub("/","_",folder[1]),".txt")
outfile <- fileDWD(outfile, quiet=quiet)
write.table(f, file=outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
# return output:
return(f)
}
