# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Weather Data Germany download with R, Climate Data Germany
# Deutscher Wetterdienst R Daten download Klimastationen
# For html rendered documentation, please visit
#   https://www.rdocumentation.org/packages/berryFunctions/versions/1.12.3/topics/dataDWD
#
#' Download data from the DWD CDC FTP Server
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip dataset is downloaded into \code{dir}.
#' If \code{read=TRUE}, it is also read, processed and returned as a data.frame
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
#'
#' @param file Complete file URL (including base and filename.zip) as returned by
#'             \code{\link{selectDWD}}. Can be a vector with several filenames.
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#' @param read Read the file with \code{\link{readDWD}}?
#'             If FALSE, only download is performed (and the filename returned). DEFAULT: TRUE
#' @param format Format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#'              is passed to \code{\link{Sys.sleep}} after each download
#'              to avoid getting kicked off the FTP-Server. DEFAULT: 0
#' @param progbar Logical: present a progress bar?
#'                Only works if the R package pbapply is available. DEFAULT: TRUE

# @param dir Writeable directory name where to save the downloaded file.
#            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
# @param quiet Suppress messages about directory / filename? DEFAULT: FALSE
# @param browse Logical: open repository via \code{\link{browseURL}}?
#               If TRUE, no metadata is downloaded, but the path URL is returned.
#               DEFAULT: FALSE
# @param files Logical: instead of station metadata, return a list of the
#              actually available files? Will call code{\link{indexDWD}}.
#              DEFAULT: FALSE
# @param ziponly Logical: If files=TRUE, only return the zip files, not the
#                description (Beschreibung.txt) files? DEFAULT: TRUE
# @param read Read the file with \code{\link{readDWD}}?
#             If FALSE, only download is performed (and the filename returned). DEFAULT: TRUE
# @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#              is passed to \code{\link{Sys.sleep}} after the download, so users
#              can wrap metaDWD in a loop without getting kicked off the FTP-Server. DEFAULT: 0
# @param \dots Further arguments passed to code{\link{download.file}}
#
dataDWD <- function(
file,
dir="DWDdata",
quiet=FALSE,          # ToDo: Optinally (DEFAULT) only download if file not available in dir
read=TRUE,
format=NA,
sleep=0,
progbar=TRUE
)
{
# path:
# ToDo : folder part of file name path
# base split off for indexDWD
# ------------------------------------------------------------------------------
# # open URL in internet browser:
# if(browse) { browseURL(paste0(base,path))  ; return(paste0(base,path)) }
# # list available files:
# if(files) return(indexDWD(path, base=base, ziponly=ziponly, dir=dir, quiet=quiet))
# # ------------------------------------------------------------------------------
# # meta File download
# # prepare file writing:
# owd <- dirDWD(dir, quiet=quiet)
# on.exit(setwd(owd))
# outfile <- tail(strsplit(filename, "/")[[1]], 1)
# outfile <- fileDWD(outfile, quiet=quiet)
# download.file(url=filename, destfile=outfile, quiet=TRUE)#, ...)
# # wait some time to avoid FTP bot recognition:
# if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
# #
# # File reading, processing and output:
# if(read) readDWD(file=outfile, dir="", meta=TRUE, progbar=FALSE) else outfile
# ------------------------------------------------------------------------------
# create directory to store downloaded data
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
# Optional progress bar:
progbar <- progbar & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
# loop over each filename
output <- lapply(file, function(f)
  {
  # output file name:
  outfile <- tail(strsplit(f, "/")[[1]], 1)
  outfile <- fileDWD(outfile, quiet=quiet)
  # Actual file download: # ToDo: wrap in try like in indexDWD, but maybe stop the rest of lapply
  download.file(url=f, destfile=outfile, quiet=TRUE)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  # Read the file:
  if(read) readDWD(file=outfile, format=format, meta=FALSE, dir="") else outfile
  })
if(length(file)==1) output[[1]] else output
}
