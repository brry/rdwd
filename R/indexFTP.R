#' Create a recursive index of an FTP Server
#' 
#' Create a list of all the files (in all subfolders) of an FTP server.
#' Defaults to the German Weather Service (DWD, Deutscher WetterDienst) OpenData server at
#' \url{ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/}.\cr\cr
#' The R package \code{RCurl} must be available to do this.
#' If \code{RCurl::\link[RCurl]{getURL}} fails, usually because bot access is
#' detected and denied, there will still be an output which you can pass in a
#' second run via \code{folder} to extract the remaining dirs.
#' You might want to wait a bit and set \code{sleep} to a higher value in that case. 
#' Here's an example:\cr
#' \code{gridindex <- indexFTP("", gridbase)}\cr
#' \code{gridindex <- indexFTP(gridindex, gridbase, sleep=1)}\cr
#' 
#' @details
#' It's not suggested to run this for all folders, as it can take quite some time
#' and you may get kicked off the FTP-Server. This package contains an index
#' of the climatic observations at weather stations: \code{View(rdwd:::\link{fileIndex})}.
#' If it is out of date, please let me know!
#' 
#' @return a vector with file paths
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{createIndex}}, \code{\link{updateIndexes}}
#' @keywords file
#' @importFrom stats runif
#' @importFrom pbapply pblapply
#' @importFrom utils write.table read.table
#' @importFrom berryFunctions removeSpace traceCall newFilename truncMessage
#' @export
#' @examples
#' \dontrun{ ## Needs internet connection
#' sol <- indexFTP(folder="/daily/solar", dir=tempdir())
#' head(sol)
#' 
#' # mon <- indexFTP(folder="/monthly/kl", dir=tempdir(), verbose=TRUE)
#' }
#' 
#' @param folder  Folder(s) to be indexed recursively, e.g. "/hourly/wind/".
#'                Leading slashes will be removed. 
#'                Use \code{folder=""} to search at the location of \code{base} itself.
#'                If \code{folder} is "currentfindex" (the default) and \code{base} 
#'                is the default, \code{folder} is changed to all observational 
#'                folders listed in the current tree file at 
#'                \url{ftp://opendata.dwd.de/weather/tree.html}. With "currentgindex" 
#'                and \code{gridbase}, the grid folders in the tree are used.
#'                DEFAULT: "currentfindex"
#' @param base    Main directory of FTP server. Trailing slashes will be removed. 
#'                DEFAULT: \code{\link{dwdbase}}
#' @param is.file.if.has.dot Logical: if some of the input paths contain a dot, 
#'                treat those as files, i.e. do not try to read those as if they
#'                were a folder. Only set this to FALSE if you know what you're doing.
#'                DEFAULT: TRUE
#' @param exclude.latest.bin Exclude latest file at opendata.dwd.de/weather/radar/radolan?
#'                RCurl::getURL indicates this is a pointer to the last regularly named file.
#'                DEFAULT: TRUE
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
#' @param quiet   Suppress progbars and message about directory/files? 
#'                DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param progbar Logical: present a progress bar in each level? DEFAULT: TRUE
#' @param verbose Logical: write a lot of messages from \code{RCurl::\link[RCurl]{getURL}}?
#'                DEFAULT: FALSE (usually, you dont need all the curl information)
#' 
indexFTP <- function(
folder="currentfindex",
base=dwdbase,
is.file.if.has.dot=TRUE,
exclude.latest.bin=TRUE,
sleep=0,
dir="DWDdata",
filename=folder[1],
overwrite=FALSE,
quiet=rdwdquiet(),
progbar=!quiet,
verbose=FALSE
)
{
# Check if RCurl is available:
checkSuggestedPackage("RCurl", "rdwd::indexFTP")
if(grepl("^https", base)) warning("base should start with ftp://, not https://. base value is: ",base)
# change folder:
if(all(folder %in% c("currentfindex","currentgindex")) & base %in% c(dwdbase, gridbase))
  {
  if(!quiet) message("Reading current index tree file...")
  tree <- readLines("ftp://opendata.dwd.de/weather/tree.html")
  if(!quiet) message("Processing index tree file...")
  urlpart <- "\"https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/"
  if(folder=="currentgindex") 
     urlpart <- sub("observations_germany/climate", "grids_germany", urlpart)
  tree <- grep(urlpart, tree, value=TRUE)
  tree <- sapply(strsplit(tree, "href="), "[", 2)
  tree <- sub(urlpart, "/", tree)
  tree <- sapply(strsplit(tree, "/\""), "[", 1)
  tree <- tree[-1]
  # Remove intermediate folders:
  tree <- tree[!tree %in% dirname(tree)]
  folder <- tree
  }
if(!quiet) message("Determining the content of the ",length(folder)," folder(s)...")
if(base!=dwdbase)
 if(missing(folder)) warning('base is not the rdwd default. It is likely you want',
                             ' to use folder="" instead of "',folder,'".')
# Progress bar?
if(progbar) lapply <- pbapply::pblapply
# single RCurl handle for all FTP requests:
curl_handle <- RCurl::getCurlHandle(ftp.use.epsv=TRUE)

# remove trailing slashes in base and leading slashes in folder:
while(grepl("/$", base)) base <- sub("/$", "", base)
while(any(grepl("^/", folder))) folder <- sub("^/","",folder) 

# central object: df_ff (dataframe with file/folder names)
df_ff <- data.frame(path=folder, isfile=FALSE, stringsAsFactors=FALSE)

if(is.file.if.has.dot)
   df_ff$isfile[ grepl(".", df_ff$path, fixed=TRUE) ] <- TRUE

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
     if(grepl("Server denied you to change to the given directory", p)) 
       p <- paste0(p,"\nThis could mean the path is a file, not a folder",
                   " or that it doesn't exist at base\n", base)
     msg <- paste0(traceCall(3, "", ": "), "RCurl::getURL failed for '", 
                   ff_row$path, "/' with error:\n - ", p)
     warning(msg, call.=FALSE)
     assign("stoppp_ffe", TRUE, inherits=TRUE) # to get out of the loop sans error
     return(ff_row) # exit getURL_ffe
    }
 # Process vector of sub-paths:
 # carriage return / newline is OS-dependent:
 p <- unlist(strsplit(p,"[\n\r]")) # http://stackoverflow.com/a/40763124/1587132
 p <- p[nchar(p)>0]
 # handle opendata.dwd.de/weather/radar/radolan/* latest data
 ilf <- grep("latest-dwd---bin", p) # index of latest file
 if(length(ilf)>0) 
   if(exclude.latest.bin) p <- p[-ilf]  else  
   p[ilf] <- sub(" -> .*", "", p[ilf]) # to keep p suited for read.text
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

stoppp_ffe <- FALSE
# as long as df_ff contains folders, run the following:
while(any(!df_ff$isfile))           # potential ToDo: exclude previously checked empty folders
  {
  df_ff <- unique(df_ff)
  df_ff1 <- df_ff[df_ff$isfile,] # these are finished
  df_ff2 <- df_ff[!df_ff$isfile,]
  df_ff3 <- lapply(1:nrow(df_ff2), function(r) getURL_ffe(df_ff2[r,])) # for the folders, run getURL
  df_ff <- do.call(rbind, c(list(df_ff1),df_ff3))
  if(stoppp_ffe) break
  } # end while loop

if(anyDuplicated(df_ff$path)) warning("Duplicate paths:", 
                   truncMessage(df_ff$path[duplicated(df_ff$path)], prefix=""))

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

