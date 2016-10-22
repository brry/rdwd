# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Deutscher Wetterdienst R Daten download Klimastationen
# Weather Data Germany download with R, Climate Data Germany
# For html rendered documentation, please visit
#   https://www.rdocumentation.org/packages/berryFunctions/versions/1.12.3/topics/dataDWD
#
#' Download data from the DWD CDC FTP Server
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip (or .txt) dataset is downloaded into \code{dir}.
#' If \code{read=TRUE}, it is also read, processed and returned as a data.frame.
#' All arguments (except for \code{dir},\code{progbar} and \code{sleep})
#' can be a vecor and will be recycled to the length of \code{file}.
#'
#' @return presuming downloading and processing were successfull:
#'         if \code{read=TRUE}, a data.frame of the desired dataset
#'         (as returned by \code{\link{readDWD}}),
#'         otherwise the filenames as saved on disc
#'         (may have "_n" appended in name, see \code{\link{fileDWD}}).\cr
#'         if length(file)>1, the output is a list of data.frames / filenames.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun-Oct 2016
#' @seealso \code{\link{readDWD}}, \code{\link{download.file}}.
#'          Helpful for plotting: \code{berryFunctions::\link[berryFunctions]{monthAxis}},
#'          see also \code{berryFunctions::\link[berryFunctions]{climateGraph}}
#' @keywords data file
#' @importFrom utils tail download.file browseURL
#' @export
#' @examples
#' # toDo: get from berryFunctions::dataDWD
#' # d <- dataDWD(selectDWD(id="05692", res="daily", var="kl", time="recent"))
#'
#' @param file   Char: complete file URL (including base and filename.zip) as returned by
#'               \code{\link{selectDWD}}. Can also be a vector with several filenames.
#' @param dir    Single char: Writeable directory name where to save the downloaded file.
#'               Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param progbar Single logical: present a progress bar?
#'               Only works if the R package \code{pbapply} is available. DEFAULT: TRUE
#' @param sleep  Single number. If not 0, a random number of seconds between 0 and
#'               \code{sleep} is passed to \code{\link{Sys.sleep}} after each download
#'               to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param quiet  Logical: suppress message about directory / filenames? DEFAULT: FALSE
#' @param meta   Logical: is the \code{file} a meta file?
#'               DEFAULT: TRUE for each file ending in ".txt"
#' @param read   Logical: read the file with \code{\link{readDWD}}? If FALSE,
#'               only download is performed and the filename returned. DEFAULT: TRUE
#' @param format Char: format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param browse Logical: open repository via \code{\link{browseURL}}? If TRUE,
#'               no data is downloaded, but the URL path without filename is returned.
#'               DEFAULT: FALSE
#' @param \dots  Further arguments passed to code{\link{download.file}}
#
dataDWD <- function(
file,
dir="DWDdata",
progbar=TRUE,
sleep=0,
quiet=FALSE,          # ToDo: Optinally (DEFAULT) only download if file not available in dir
meta=substr(file, nchar(file)-3, 1e4)==".txt",
read=TRUE,
format=NA,
browse=FALSE,
...
)
{
len <- length(file)
if(missing(progbar) & len==1) progbar <- FALSE
# recycle input vectors
if(len>1)
  {
  quiet  <- rep(quiet,  length.out=len)
  meta   <- rep(meta,   length.out=len)
  read   <- rep(read,   length.out=len)
  format <- rep(format, length.out=len)
  browse <- rep(browse, length.out=len)
  }
# output file name(s)
outnames <- gsub("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/", "", file)
outnames <- gsub("/", "_", outnames)
# ------------------------------------------------------------------------------
# create directory to store downloaded data
owd <- dirDWD(dir, quiet=quiet[1])
on.exit(setwd(owd))
# Optional progress bar:
progbar <- progbar[1] & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
# loop over each filename
output <- lapply(seq_along(file), function(i)
  {
  # open URL in internet browser:
  if(browse[i]) { browseURL(dirname(file))  ; return(dirname(file)) }
  # output file name:
  outfile <- fileDWD(outnames[i], quiet=quiet[i])
  # Actual file download: # ToDo: wrap in try like in indexDWD, but maybe stop the rest of lapply
  download.file(url=file[i], destfile=outfile, quiet=quiet[i], ...)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  # Output: Read the file or outfile name:
  if(read[i]) readDWD(file=outfile, dir="", meta=meta[i], format=format[i]) else outfile
  })
#
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}
