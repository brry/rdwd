#' Process data from the DWD CDC FTP Server
#'
#' Read climate (meta) data that was downloaded with \code{\link{dataDWD}}.
#' The file is read, processed and returned as a data.frame.\cr
#' If \code{meta=TRUE}: column widths for \code{\link{read.fwf}} are computed internally.
#'
#' @return data.frame of the desired dataset
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{selectDWD}}
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf
#' @importFrom berryFunctions checkFile na9
#' @export
#' @examples
#' # see dataDWD
# ToDo: read station id as character string by specifying all column classes, see
# http://stackoverflow.com/questions/13022299/specify-date-format-for-colclasses-argument-in-read-table-read-csv/13022441#13022441
#'
#' @param file Name(s) of the file(s) downloaded with \code{\link{dataDWD}},
#'             e.g. "tageswerte_KL_02575_akt.zip" or
#'             "RR_Stundenwerte_Beschreibung_Stationen.txt"
#' @param dir Directory name where to read the file. Use \code{"."} or \code{""}
#'            if \code{file} already includes a (relative) path.
#'            DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param meta Logical: is the \code{file} a meta file?
#'                      DEFAULT: TRUE for each file ending in ".txt"
#' @param format If \code{meta=FALSE}: Format passed to \code{\link{as.POSIXct}}
#'               (see \code{\link{strptime}})
#'               to convert the date/time column to POSIX time format.\cr
#'               If NULL, no conversion is performed (date stays a factor).
#'               If NA, \code{readDWD} tries to find suitable format based on the number of characters.
#' @param progbar ToDo
#'
readDWD <- function(
file,
dir="DWDdata",
meta=substr(file, nchar(file)-3, 1e4)==".txt",
format=NA,
progbar=TRUE
)
{
# set directory from which to read downloaded data
owd <- dirDWD(dir, quiet=TRUE)
on.exit(setwd(owd))
# Optional progress bar:
progbar <- progbar & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
# file check, type selection:
f <- file[i]
checkFile(f)
if(!meta[i]) # -----------------------------------------------------------------
{
# temporary unzipping directory
exdir <- paste0(tempdir(),"/", substr(f, 1, nchar(f)-4))
unzip(f, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE))
# new filename - the actual data file:
f <- dir(exdir, pattern="produkt*", full.names=TRUE)
###if(length(f)!=1) warning("There is more/less than 1 'produkt*' f in ", exdir,
###                            ":\n", toString(f), "\nOnly the first one is used.")
# Actually read data
dat <- read.table(f, na.strings=na9(), header=TRUE, sep=";", as.is=FALSE)
# process time-stamp:
if(!is.null(format) & "MESS_DATUM" %in% colnames(dat))
  {
  if(is.na(format)) format <- if(nchar(dat$MESS_DATUM[1])==8) "%Y%m%d" else "%Y%m%d%H"
  dat$MESS_DATUM <- as.POSIXct(as.character(dat$MESS_DATUM), format=format)
  }
# return dataset:
return(dat)
} else # if meta ---------------------------------------------------------------
{
widths <- c( 6,9,9,15,12,10,41,100)
# read one line to confirm widths and get column names
oneline <- readLines(f, n=3)
if(substr(oneline[3],1,6)=="      ") widths[1] <- 11
# actually read metadata, suppress readLines warning about EOL:
stats <- suppressWarnings(read.fwf(f, widths=widths, skip=2, strip.white=TRUE) )
# column names:
colnames(stats) <- strsplit(oneline[1], " ")[[1]]
# return meta data.frame:
return(stats)
}
# lapply loop end
})
if(length(file)==1) output[[1]] else output
}
