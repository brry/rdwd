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
#' @return Presuming downloading and processing were successfull:
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
#' @importFrom berryFunctions newFilename
#' @export
#' @examples
#' \dontrun{ ## requires internet connection
#' # find files for a given station name and file path:
#' link <- selectDWD("Kupferzell-Rechbach", res="hourly", var="precipitation", per="recent")
#' # actually download and read files
#' prec <- dataDWD(link, dir="DWDdata") # the default dir
#' fname <- dataDWD(link, read=FALSE) # filename, no second download (unless force=TRUE)
#' 
#' # current and historical files:
#' link <- selectDWD("Potsdam", res="daily", var="kl", per="hr", outvec=TRUE); link
#' potsdam <- dataDWD(link)
#' potsdam <- do.call(rbind, potsdam) # this will partly overlap in time
#' plot(LUFTTEMPERATUR~MESS_DATUM, data=tail(potsdam,1000), type="l")
#' # Straight line marks the jump back in time
#' # check for equality:
#' dup <- which(duplicated(potsdam$MESS_DATUM))
#' dup_df <- which(duplicated(potsdam))
#' err <- dup[ ! dup %in% dup_df]
#' err <- potsdam[potsdam$MESS_DATUM %in% potsdam$MESS_DATUM[err], ]
#' err <- err[order(err$MESS_DATUM),]
#' View(err) # WINDGESCHWINDIGKEIT (wind speed) has been slightly changed
#' # Keep only historical dataset:
#' potsdam <- potsdam[!duplicated(potsdam$MESS_DATUM),]
#' 
#' # several files:
#' link <- c(link, selectDWD("Potsdam", res="daily", var="kl", per="hr", outvec=TRUE))
#' clim <- dataDWD(link)
#' fname <- dataDWD(link, read=FALSE)
#' clim <- readDWD(fname)
#' unzip(zipfile=paste0("DWDdata/",fname[1]), exdir="DWDdata/Testunzip")
#' # There's quite some important meta information there!
#' 
#' plot(prec$MESS_DATUM, prec$NIEDERSCHLAGSHOEHE, main="DWD hourly rain Kupferzell", col="blue",
#'      xaxt="n", las=1, type="l", xlab="Date", ylab="Hourly rainfall  [mm]")
#' monthAxis(1, ym=T)
#' 
#' d <- dataDWD(selectDWD(id="05692", res="daily", var="kl", per="recent"))
#' # writes into the same folder (dir="DWDdata")
#' 
#' folder <- dataDWD(link, browse=T)
#' folder
#' 
#' # With many files, use sleep
#' links <- selectDWD(res="daily", var="solar", meta=FALSE)
#' sol <- dataDWD(links, sleep=20) # random waiting time after download (0 to 20 secs)
#' 
#' # Real life example with data completeness check etc:
#' browseURL("http://github.com/brry/prectemp/blob/master/Code_example.R")
#' 
#' }
#' 
#' @param file   Char (vector): complete file URL(s) (including base and filename.zip) as returned by
#'               \code{\link{selectDWD}}. Can be a vector with several filenames.
#' @param dir    Char: Writeable directory name where to save the downloaded file.
#'               Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param force  Logical: always download, even if the file already exists in \code{dir}?
#'               If FALSE, it is still read (or name returned). DEFAULT: FALSE
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
#' @param fread  Fast reading? See \code{\link{readDWD}}. DEFAULT: NA
#' @param format Char (vector): format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param ntrunc Single integer: number of filenames printed in messages
#'               before they get truncated with message "(and xx more)". DEFAULT: 2
#' @param \dots  Further arguments passed to \code{\link{download.file}}
#
dataDWD <- function(
file,
dir="DWDdata",
force=FALSE,
sleep=0,
quiet=FALSE,
progbar=!quiet,
browse=FALSE,
read=TRUE,
meta=substr(file, nchar(file)-3, 1e4)==".txt",
fread=NA,
format=NA,
ntrunc=2,
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
outfile <- gsub("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/", "", file)
outfile <- gsub("/", "_", outfile)
dontdownload <- file.exists(outfile) & !force
if( any(dontdownload) & !quiet )
  {
  message(traceCall(1, "", ": "), sum(dontdownload), " file", if(sum(dontdownload)>1)"s",
          " already existing and not downloaded again: ",
          berryFunctions::truncMessage(outfile[dontdownload], ntrunc=ntrunc, prefix=""),
          "\nNow downloading ",sum(!dontdownload)," files...")
  }
outfile <- newFilename(outfile, quiet=quiet, ignore=dontdownload, ntrunc=ntrunc)
# since berryFunctions 1.15.9 (2017-06-14), outfile is now an absolute path
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
# ------------------------------------------------------------------------------
# loop over each filename
lapply(seq_along(file), function(i)
  if(!dontdownload[i])
  {
  # Actual file download:
  download.file(url=file[i], destfile=outfile[i], quiet=TRUE, ...)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  })
# ------------------------------------------------------------------------------
# Output: Read the file or outfile name:
output <- outfile
if(read)
  {
  if(progbar) message("Reading ", length(outfile), " file", if(length(outfile)>1)"s", "...")
  output <- readDWD(file=outfile, meta=meta, fread=fread, format=format, progbar=progbar)
  }
# output:
return(invisible(output))
}
