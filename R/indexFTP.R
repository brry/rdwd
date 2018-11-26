#' Create a recursive index of the DWD CDC FTP Server
#' 
#' Create a list of all the files (in subfolders) at the Climate Data Center (CDC)
#' FTP-Server from the German Weather Service (DWD, Deutscher WetterDienst) at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}.\cr\cr
#' The R package \code{RCurl} must be available to do this.
#' If \code{RCurl::\link[RCurl]{getURL}} fails, usually because bot access is
#' detected and denied, there will still be an output which you can pass in a
#' second run via \code{folder} to extract the remaining dirs.
#' You might want to wait a bit and set \code{sleep} to a higher value in that case. 
#' Here's an example:\cr
#' \code{gridindex <- indexFTP("grids_germany","ftp://ftp-cdc.dwd.de/pub/CDC")}\cr
#' \code{gridindex <- indexFTP(gridindex,"ftp://ftp-cdc.dwd.de/pub/CDC", sleep=1)}\cr
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
#' @importFrom pbapply pblapply
#' @importFrom utils write.table
#' @importFrom berryFunctions removeSpace traceCall newFilename
#' @export
#' @examples
#' \dontrun{ ## Needs internet connection
#' sol <- indexFTP(folder="/daily/solar")
#' head(sol)
#' 
#' mon <- indexFTP(folder="/monthly/kl", verbose=TRUE)
#' }
#' 
#' @param folder  Folder(s) to be indexed recursively, e.g. "/hourly/wind/".
#'                Use \code{folder=""} to search at the location of \code{base} itself.
#'                If code{folder} is "currentfindex" (the default) and \code{base} 
#'                is the default, code{folder} is changed to all folders in current 
#'                \code{\link{fileIndex}}: \code{unique(dirname(fileIndex$path))}. 
#'                DEFAULT: "currentfindex"
#' @param base    Main directory of DWD ftp server, defaulting to observed climatic records.
#'                DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param sleep   If not 0, a random number of seconds between 0 and \code{sleep}
#'                is passed to \code{\link{Sys.sleep}} after each read folder
#'                to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param dir     Writeable directory name where to save the downloaded file.
#'                Created if not existent.
#'                DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param filename Character: Part of output filename. "INDEX_of_DWD_" is prepended,
#'                "/" replaced with "_", ".txt" appended. DEFAULT: folder[1]
#' @param overwrite Logical: Overwrite existing file? If not, "_n" is added to the
#'                filename, see \code{berryFunctions::\link[berryFunctions]{newFilename}}.
#'                DEFAULT: FALSE
#' @param quiet   Suppress progbars and message about directory/files? DEFAULT: FALSE
#' @param progbar Logical: present a progress bar in each level?
#'                Only works if the R package pbapply is available. DEFAULT: TRUE
#' @param verbose Logical: write a lot of messages from \code{RCurl::\link[RCurl]{getURL}}?
#'                DEFAULT: FALSE (usually, you dont need all the curl information)
#' 
indexFTP <- function(
folder="currentfindex",
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
sleep=0,
dir="DWDdata",
filename=folder[1],
overwrite=FALSE,
quiet=FALSE,
progbar=!quiet,
verbose=FALSE
)
{
# Check if RCurl is available:
if(!requireNamespace("RCurl", quietly=TRUE))
  stop("The R package 'RCurl' is not available. rdwd::indexFTP can not obtain file list.\n",
       "install.packages('RCurl')       to enable this.")
# change folder:
if(all(folder=="currentfindex") & base=="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate")
   folder <- unique(dirname(fileIndex$path))
if(base!="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate")
 if(missing(folder)) warning('base is not the rdwd default. It is likely you want',
                             ' to use folder="" instead of "',folder,'".')
# Progress bar?
if(progbar) lapply <- pbapply::pblapply
# single RCurl handle for all FTP requests:
curl_handle <- RCurl::getCurlHandle(ftp.use.epsv=TRUE)


# central object: df_ff (dataframe with file/folder names)
df_ff <- data.frame(path=folder, isfile=FALSE, stringsAsFactors=FALSE)


# List files at a single path, returning files/folders or useful error messages
# stoppp_ffe will be an object within indexFTP, see below
getURL_ffe <- function(ff_row) 
 {
 if(stoppp_ffe) return(ff_row) # do not attempt if already kicked off the FTP
 p <- try( RCurl::getURL(paste0(base,"/",ff_row$path,"/"), verbose=verbose, 
                         ftp.use.epsv=TRUE, curl=curl_handle),      silent=TRUE)
 # upon failure, exit with a warning (not error):
 if(inherits(p, "try-error"))
    {
     # Prepare warning message text:
     p <- gsub("Error in function (type, msg, asError = TRUE)  : ", "", p, fixed=TRUE)
     p <- removeSpace(gsub("\n", "", p))
     if(grepl("Could not resolve host", p)) 
       p <- paste0(p,"\nThis may mean you are not connected to the internet.")
     msg <- paste0(traceCall(3, "", ": "), "RCurl::getURL failed for '", ff_row$path, "/' - ", p)
     warning(msg, call.=FALSE)
     assign("stoppp_ffe", TRUE, inherits=TRUE) # to get out of the loop sans error
     return(ff_row) # exit getURL_ffe
    }
 # Process vector of sub-paths:
 # carriage return / newline is OS-dependent:
 p <- unlist(strsplit(p,"[\n\r]")) # http://stackoverflow.com/a/40763124/1587132
 p <- p[nchar(p)>0]
 #
 isdir <- substr(p,1,1) =="d" # directory, else file
 pnames <- read.table(text=p, stringsAsFactors=FALSE)
 pnames <- pnames[,ncol(pnames)] # only use last column with path names
 output <- data.frame(path=paste0(ff_row$path,"/",pnames), isfile=!isdir, stringsAsFactors=FALSE)
 #
 # wait some time if needed:
 if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
 # complete file path:
 return(output)
}


# as long as df_ff contains folders, run the following:
stoppp_ffe <- FALSE
while(any(!df_ff$isfile))           # potential ToDo: exclude previously checked empty folders
  {
  df_ff1 <- df_ff[df_ff$isfile,] # these are finished
  df_ff2 <- df_ff[!df_ff$isfile,]
  df_ff3 <- lapply(1:nrow(df_ff2), function(r) getURL_ffe(df_ff2[r,])) # for the folders, run getURL
  df_ff <- do.call(rbind, c(list(df_ff1),df_ff3))
  #duplicated(df_ff$path)
  } # end while loop

# sort final results alphabetically (path only, no f/f info):
finalpaths <- sort(df_ff$path)

# write output:
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
outfile <- paste0("INDEX_of_DWD_", gsub("/","_",filename),".txt")
outfile <- newFilename(outfile, ignore=overwrite, pre="", mid="", quiet=quiet)
write.table(finalpaths, file=outfile, row.names=FALSE, col.names=FALSE, quote=FALSE)
# return output:
return(finalpaths)
}

