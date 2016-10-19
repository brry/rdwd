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
#' @return data.frame of the desired dataset (returned by \code{\link{readDWD}} if meta=0),
#'         presuming downloading and processing were successfull.
#'         Alternatively, links that were opened if \code{browse}!=0.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{readDWD}}, \code{\link{download.file}}.
#'          Helpful for plotting: \code{berryFunctions::\link[berryFunctions]{monthAxis}},
#'          see also \code{berryFunctions::\link[berryFunctions]{climateGraph}}
#' @keywords data file
#' @importFrom utils tail download.file
#' @export
#' @examples
#' # toDo: get from berryFunctions::dataDWD
#'
#' @param file Complete file URL (including filename.zip) as returned by
#'             \code{\link{metaDWD}} if \code{id} or \code{name} is given.
#'             Can be a vector with several filenames.
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#' @param read Read the file with \code{\link{readDWD}}?
#'             If FALSE, only download is performed. DEFAULT: TRUE
#' @param format Format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param sleep If not 0, a random number of seconds between 0 and \code{sleep}
#'              is passed to \code{\link{Sys.sleep}} after each download
#'              to avoid getting kicked off the FTP-Server. DEFAULT: 0
#'
dataDWD <- function(
file,
dir="DWDdata",
quiet=FALSE,
read=TRUE,
format=NA,
sleep=0
)
{
# create directory to store downloaded data
owd <- dirDWD(dir)
on.exit(setwd(owd))
# loop over each filename # toDo: progress bar
output <- lapply(file, function(f)
  {
  # output file name:
  outfile <- tail(strsplit(f, "/")[[1]], 1)
  # Actual file download:
  download.file(url=f, destfile=outfile, quiet=TRUE)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  # Read the file:
  if(read) readDWD(file=outfile, format=format)
  })
if(length(file)!=1) output[[1]] else output
}
