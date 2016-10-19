#' Process data from the DWD CDC FTP Server
#'
#' Read climate data that was downloaded with \code{\link{dataDWD}}.
#' The file is read, processed and returned as a data.frame
#'
#' @return data.frame of the desired dataset
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file chron
#' @importFrom utils read.table unzip
#' @importFrom berryFunctions checkFile na9
#' @export
#' @examples
#' # see dataDWD
#'
#' @param file Name of Zip-File downloaded with \code{\link{dataDWD}},
#'             e.g. "tageswerte_KL_02575_akt.zip".
#' @param format Format passed to \code{\link{as.POSIXct}} (see \code{\link{strptime}})
#'               to convert the date/time column to POSIX time format .
#'               If NULL, no conversion is performed (date stays a factor).
#'               If NA, \code{readDWD} tries to find suitable format based on the number of characters.
#'
readDWD <- function(
file,
format=NA
)
{
# file selection
checkFile(file)
exdir <- paste0(tempdir(),"/", substr(file, 1, nchar(file)-4))
unzip(file, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE))
# new filename - the actual data file:
file <- dir(exdir, pattern="produkt*", full.names=TRUE)
###if(length(file)!=1) warning("There is more/less than 1 'produkt*' file in ", exdir,
###                            ":\n", toString(file), "\nOnly the first one is used.")
###file <- file[1]
# Actually read data
dat <- read.table(file, na.strings=na9(), header=TRUE, sep=";", as.is=FALSE)
# process time-stamp:
if(!is.null(format) & "MESS_DATUM" %in% colnames(dat))
  {
  if(is.na(format)) format <- if(nchar(dat$MESS_DATUM[1])==8) "%Y%m%d" else "%Y%m%d%H"
  dat$MESS_DATUM <- as.POSIXct(as.character(dat$MESS_DATUM), format=format)
  }
# return dataset:
dat
}
