#' Process data from the DWD CDC FTP Server
#'
#' Read climate (meta) data that was downloaded with \code{\link{dataDWD}}.
#' The file is read, processed and returned as a data.frame.\cr
#' If \code{meta=TRUE}: column widths for \code{\link{read.fwf}} are computed internally.\cr
#' \code{file} can be a vector with several filenames. The arguments \code{meta}
#' and \code{format} can also be a vecor and will be recycled to the length of \code{file}.
#'
#' @return Invisible data.frame of the desired dataset, or a list of data.frames
#'         if length(file) > 1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{selectDWD}}
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf
#' @importFrom berryFunctions checkFile na9 traceCall
#' @export
#' @examples
#' # see dataDWD
#'
#' @param file   Char (vector): name(s) of the file(s) downloaded with \code{\link{dataDWD}},
#'               e.g. "tageswerte_KL_02575_akt.zip" or
#'               "RR_Stundenwerte_Beschreibung_Stationen.txt"
#' @param dir    Char: directory name where to read the file. Use \code{"."} or \code{""}
#'               if \code{file} already includes a (relative or absolute) path.
#'               DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param meta   Logical (vector): is the \code{file} a meta file?
#'               DEFAULT: TRUE for each file ending in ".txt"
#' @param format Char (vector): if \code{meta=FALSE}: Format passed to
#'               \code{\link{as.POSIXct}} (see \code{\link{strptime}})
#'               to convert the date/time column to POSIX time format.\cr
#'               If NULL, no conversion is performed (date stays a factor).
#'               If NA, \code{readDWD} tries to find suitable format based
#'               on the number of characters. DEFAULT: NA
#' @param tz     Char (vector): time zone for \code{\link{as.POSIXct}}.
#'               "" is the current time zone, and "GMT" is UTC (Universal Time,
#'               Coordinated). DEFAULT: "GMT"
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               Only works if the R package \code{pbapply} is available. DEFAULT: TRUE
#'
readDWD <- function(
file,
dir="DWDdata",
meta=substr(file, nchar(file)-3, 1e4)==".txt",
format=NA,
tz="GMT",
progbar=TRUE
)
{
# recycle meta, format and tz
len <- length(file)
if(missing(progbar) & len==1) progbar <- FALSE
if(len>1)
  {
  meta   <- rep(meta,   length.out=len)
  format <- rep(format, length.out=len)
  tz     <- rep(tz,     length.out=len)
  }
# set directory from which to read downloaded data
owd <- dirDWD(dir, quiet=TRUE)
on.exit(setwd(owd))
# Optional progress bar:
progbar <- progbar & requireNamespace("pbapply", quietly=TRUE)
if(progbar) lapply <- pbapply::pblapply
#
checkFile(file)
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
f <- file[i]
if(!meta[i]) # if data ---------------------------------------------------------
{
# temporary unzipping directory
exdir <- paste0(tempdir(),"/", substr(f, 1, nchar(f)-4))
unzip(f, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE))
# new filename - the actual data file:
f <- dir(exdir, pattern="produkt*", full.names=TRUE)
# Actually read data
dat <- read.table(f, na.strings=na9(), header=TRUE, sep=";", as.is=FALSE)
##
## # The alternative would be to use
## fn <- unzip(f, list=TRUE)
## fn <- fn$Name[grep(pattern="produkt", x=fn$Name)]
## dat <- read.table(unz(f,fn), na.strings=na9(), header=TRUE, sep=";", as.is=FALSE)
## # but it is not faster and introduces incomplete NA rows at the data.frame end
## # and also removes the opportunity to potentially add an argument cleanup=FALSE
##
# process time-stamp: http://stackoverflow.com/a/13022441
if(!is.null(format[i]) & "MESS_DATUM" %in% colnames(dat))
  {
  if(is.na(format[i])) format <- if(nchar(dat$MESS_DATUM[1])==8) "%Y%m%d" else "%Y%m%d%H"
  dat$MESS_DATUM <- as.POSIXct(as.character(dat$MESS_DATUM), format=format, tz=tz[i])
  }
# return dataset:
return(dat)
} else # if meta ---------------------------------------------------------------
{
# read one line to get column widths and names
oneline <- readLines(f, n=3)
# column widhts (automatic detection across different styles used by the DWD)
spaces <- unlist(gregexpr(" ", oneline[3]))
breaks <- spaces[which(diff(spaces)!=1)]
if(substr(oneline[3],1,1)==" ") breaks <- breaks[-1]
breaks[3] <- breaks[3] -9 # right-adjusted column
breaks[4:5] <- breaks[4:5] -1 # right-adjusted columns
widths <- diff(c(0,breaks,200))
# actually read metadata, suppress readLines warning about EOL:
stats <- suppressWarnings(read.fwf(f, widths=widths, skip=2, strip.white=TRUE) )
# column names:
colnames(stats) <- strsplit(oneline[1], " ")[[1]]
# check classes:
classes <- c("integer", "integer", "integer", "integer", "numeric", "numeric", "factor", "factor")
actual <- sapply(stats, class)
if(actual[4]=="numeric") classes[4] <- "numeric"
if(!all(actual == classes))
  {
  msg <- paste0(names(actual)[actual!=classes], ": ", actual[actual!=classes],
                " instead of ", classes[actual!=classes], ".")
  msg <- paste(msg, collapse=" ")
  warning(traceCall(1, "", ": "), "reading file '", f,
          "' did not give the correct column classes. ", msg, call.=FALSE)
  }
# return meta data.frame:
return(stats)
}
# lapply loop end
})
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}
