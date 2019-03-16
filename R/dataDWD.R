# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Deutscher Wetterdienst R Daten download Klimastationen
# Weather Data Germany download with R, Climate Data Germany
#
#' Download data from the DWD CDC FTP Server
#' 
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip (or .txt) dataset is downloaded into \code{dir}.
#' If \code{read=TRUE}, it is also read, processed and returned as a data.frame.
#' 
#' @return Presuming downloading and processing were successful:
#'         if \code{read=TRUE}, a data.frame of the desired dataset
#'         (as returned by \code{\link{readDWD}}),
#'         otherwise the filename as saved on disc
#'         (may have "_n" appended in name, see \code{\link{newFilename}}).\cr
#'         If length(file)>1, the output is a list of data.frames / vector of filenames.\cr
#'         The output is always invisible.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun-Oct 2016
#' @seealso \code{\link{selectDWD}}. \code{\link{readDWD}}, \code{\link{download.file}}.
#'          Helpful for plotting: \code{berryFunctions::\link[berryFunctions]{monthAxis}},
#'          see also \code{berryFunctions::\link[berryFunctions]{climateGraph}}
#' @keywords data file
#' @importFrom utils tail download.file browseURL
#' @importFrom berryFunctions newFilename owa traceCall truncMessage
#' @importFrom pbapply pblapply
#' @importFrom stats runif
#' @export
#' @examples
#' \dontrun{ ## requires internet connection
#' # find FTP files for a given station name and file path:
#' link <- selectDWD("Fuerstenzell", res="hourly", var="wind", per="recent")
#' # download file:
#' fname <- dataDWD(link, dir=tempdir(), read=FALSE) ; fname
#' # dir="DWDdata" is the default directory to store files
#' # unless force=TRUE, already obtained files will not be downloaded again
#' 
#' # read and plot file:
#' wind <- readDWD(fname)          ; head(wind)
#' metafiles <- readMeta(fname)    ; str(metafiles, max.level=1)
#' column_names <- readVars(fname) ; head(column_names)
#' 
#' plot(wind$MESS_DATUM, wind$F, main="DWD hourly wind Fuerstenzell", col="blue",
#'      xaxt="n", las=1, type="l", xlab="Date", ylab="Hourly Wind speed  [m/s]")
#' berryFunctions::monthAxis(1, ym=T)
#' 
#' 
#' # current and historical files:
#' link <- selectDWD("Potsdam", res="daily", var="kl", per="hr"); link
#' potsdam <- dataDWD(link, dir=tempdir())
#' potsdam <- do.call(rbind, potsdam) # this will partly overlap in time
#' plot(TMK~MESS_DATUM, data=tail(potsdam,1500), type="l")
#' # The straight line marks the jump back in time
#' # Keep only historical data in the overlap time period:
#' potsdam <- potsdam[!duplicated(potsdam$MESS_DATUM),]
#' 
#' 
#' # With many files (>>50), use sleep to avoid getting kicked off the FTP server
#' #links <- selectDWD(res="daily", var="solar")
#' #sol <- dataDWD(links, sleep=20) # random waiting time after download (0 to 20 secs)
#' 
#' # Real life example with data completeness check etc:
#' browseURL("https://github.com/brry/prectemp/blob/master/Code_analysis.R")
#' 
#' }
#' 
#' @param file   Char (vector): complete file URL(s) (including base and filename.zip) as returned by
#'               \code{\link{selectDWD}}. Can be a vector with several filenames.
#' @param base   Single char: base URL that will be removed from output file names.
#'               DEFAULT: \code{\link{dwdbase}}:
#'               \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param dir    Char: Writeable directory name where to save the downloaded file.
#'               Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param force  Logical (vector): always download, even if the file already exists in \code{dir}?
#'               Use NA to force re-downloading files older than 24 hours.
#'               If FALSE, it is still read (or name returned). DEFAULT: FALSE
#' @param overwrite Logical (vector): if force=TRUE, overwrite the existing file
#'               rather than append "_1"/"_2" etc to the filename? DEFAULT: FALSE
#' @param sleep  Number. If not 0, a random number of seconds between 0 and
#'               \code{sleep} is passed to \code{\link{Sys.sleep}} after each download
#'               to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param quiet  Logical: suppress message about directory / filenames? DEFAULT: FALSE
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               Only works if the R package \code{pbapply} is available. DEFAULT: TRUE (!quiet)
#' @param browse Logical: open repository via \code{\link{browseURL}} and
#'               return URL folder path? If TRUE, no data is downloaded.
#'               If file has several values, only unique folders will be opened.
#'               DEFAULT: FALSE
#' @param read   Logical: read the file(s) with \code{\link{readDWD}}? If FALSE,
#'               only download is performed and the filename(s) returned. DEFAULT: TRUE
#' @param meta   Logical (vector): is the \code{file} a meta file? Passed to
#'               \code{\link{readDWD}}. DEFAULT: TRUE for each file ending in ".txt"
#' @param fread  Fast reading? See \code{\link{readDWD}}. DEFAULT: FALSE
#' @param format Char (vector): format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param ntrunc Single integer: number of filenames printed in messages
#'               before they get truncated with message "(and xx more)". DEFAULT: 2
#' @param dfargs Named list of additional arguments passed to \code{\link{download.file}}
#' @param \dots  Further arguments passed to \code{\link{readDWD}}. 
#'               Were passed to \code{\link{download.file}} prior to rdwd 0.11.7 (2019-02-25)
#
dataDWD <- function(
file,
base=dwdbase,
dir="DWDdata",
force=FALSE,
overwrite=FALSE,
sleep=0,
quiet=FALSE,
progbar=!quiet,
browse=FALSE,
read=TRUE,
meta=grepl('.txt$', file),
fread=FALSE,
format=NA,
ntrunc=2,
dfargs=NULL,
...
)
{
if(!is.atomic(file)) stop("file must be a vector, not a ", class(file))
if(!is.character(file)) stop("file must be char, not ", class(file))
if(missing(progbar) & length(file)==1) progbar <- FALSE
if(any(file==""))
{
  message(traceCall(1, "", ": "), "Removing ", sum(file==""), " empty element(s) from file vector.")
  file <- file[file!=""]
}
if(length(file)<1) stop("The vector of files to be downloaded is empty.")
# be safe from accidental vector input:
dir     <- dir[1]
progbar <- progbar[1]
sleep   <- sleep[1]
quiet   <- quiet[1]
read    <- read[1]
browse  <- browse[1]
#
# open URL path(s) in internet browser:
if(browse)
  {
  folders <- unique(dirname(file))
  sapply(folders, browseURL)
  return(folders)
  }
# create directory to store downloaded data
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
# output file name(s)
outfile <- gsub(paste0(base,"/"), "", file)
outfile <- gsub("/", "_", outfile)

# force=NA management
force_old <- difftime(Sys.time(), file.mtime(outfile), units="h") > 24
force <- rep(force, length=length(outfile)) # recycle vector
force[is.na(force) & force_old] <- TRUE # force if old
force[is.na(force)] <- FALSE # otherwise don't

dontdownload <- file.exists(outfile) & !force
if( any(dontdownload) & !quiet )
  {
  message(traceCall(1, "", ": "), sum(dontdownload), " file", if(sum(dontdownload)>1)"s",
          " already existing and not downloaded again: ",
          berryFunctions::truncMessage(outfile[dontdownload], ntrunc=ntrunc, prefix=""),
          "\nNow downloading ",sum(!dontdownload)," files...")
  }
outfile <- newFilename(outfile, quiet=quiet, ignore=dontdownload, 
                       overwrite=overwrite, ntrunc=ntrunc, tellignore=FALSE)
# since berryFunctions 1.15.9 (2017-06-14), outfile is now an absolute path
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
# ------------------------------------------------------------------------------
# loop over each filename
dl_results <- lapply(seq_along(file), function(i)
  if(!dontdownload[i])
  {
  # Actual file download:
  dfdefaults <- list(url=file[i], destfile=outfile[i], quiet=TRUE)
  e <- try(suppressWarnings(do.call(download.file, 
                         berryFunctions::owa(dfdefaults, dfargs))), silent=TRUE)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  return(e)
  })

# check for download errors:
iserror <- sapply(dl_results, inherits, "try-error")
if(any(iserror))
  { 
  ne <- sum(iserror)
  msg <- paste0(ne, " Download", if(ne>1) "s have" else " has", 
                " failed (out of ",length(iserror),").", 
                if(read)" Setting read=FALSE.","\n")
  read <- FALSE
  if(any(substr(file[iserror], 1, 4) != "ftp:"))
     msg <- paste0(msg, "dataDWD needs urls starting with 'ftp://'.\n")
  msg <- paste0(msg, "download.file error",if(ne>1) "s",":\n")
  msg2 <- sapply(dl_results[iserror], function(e)attr(e,"condition")$message)
  msg <- paste0(msg, paste(msg2, collapse="\n"))
  warning(msg, call.=FALSE)
  }
# ------------------------------------------------------------------------------
# Output: Read the file or outfile name:
output <- outfile
if(read)
  {
  if(progbar) message("Reading ", length(outfile), " file", if(length(outfile)>1)"s", "...")
  output <- readDWD(file=outfile, meta=meta, fread=fread, format=format, progbar=progbar, ...)
  }
# output:
return(invisible(output))
}
