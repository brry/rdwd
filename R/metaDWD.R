#' Get metadata from the DWD CDC FTP Server
#'
#' Get station metadata. Useful to select files for downloading with \code{\link{dataDWD}}.
#' Column widths for \code{\link{read.fwf}} are computed internally)
#'
#' @return data.frame with weather station metadata
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{indexDWD}}, \code{\link{readDWD}}
#' @keywords file
#' @importFrom utils browseURL read.fwf write.table
#' @export
#' @examples
#' # toDo
#'
#' @param id,name Optional character strings. If !="" and corresponding
#'                file(s) exist(s), metaDWD returns "base/res/var/time/filename.zip"
#'                CURRENTLY IGNORED! ToDo: match with indexed metadata
#' @param base Main directory of DWD ftp server, defaulting to observed climatic records.
#'             DEFAULT: ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
#' @param res Time resolution available at \code{base}. usually one of
#'            hourly","daily","monthly". DEFAULT: "hourly"
#' @param var Weather variable of interest, e.g.   ToDo: put overview from index here
#'            DEFAULT: "precipitation"
#' @param time Desired time range. One of
#'             "recent" (data from the last year, up to date usually within a few days) or
#'             "historical" (long time series). Can be abbreviated (partial matching).
#'             DEFAULT: "recent" (the first of the inputs is used)
#' @param dir Writeable directory name where to save the downloaded file.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#' @param browse Logical: open repository via \code{\link{browseURL}}?
#'               If TRUE, no metadata is downloaded, but the path URL is returned.
#'               DEFAULT: FALSE
#' @param files Logical: instead of station metadata, return a list of the
#'              actually available files? Will call code{\link{indexDWD}}.
#'              DEFAULT: FALSE
#' @param ziponly Logical: If files=TRUE, only return the zip files, not the
#'                description (Beschreibung.txt) files? DEFAULT: TRUE
#'
metaDWD <- function(
id="",
name="",
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
res="hourly",
var="precipitation",
time=c("recent", "historical"),
dir="DWDdata",
quiet=FALSE,
browse=FALSE,
files=FALSE,
ziponly=TRUE
)
{
# Input checks and processing:
base <- base[1]
res  <-  res[1]
var  <-  var[1]
time <- match.arg(time)
# path:
path <- paste0(base,"/",res,"/",var,"/",time)
# path plausibility:
### Using index - toDo!
# URL existance check if RCurl available
### toDo!
# ------------------------------------------------------------------------------
# open URL in internet browser:
if(browse) { browseURL(path)  ; return(path) }
# list available files:
if(files) return(indexDWD(path, base="", ziponly=ziponly, dir=dir, quiet=quiet))
# ------------------------------------------------------------------------------
# metadata file name parts:
resname <- c(hourly="Stundenwerte", daily="Tageswerte", monthly="Monatswerte")
varname <- c(air_temperature="TU", cloudiness="N", # \
             precipitation="RR", pressure="P0",    #  |-  only in hourly
             sun="SD", wind="FF",                  # /
             soil_temperature="EB", solar="ST",    # hourly and daily
             kl="KL", more_precip="RR")            # daily and monthly
# resolution name part:
resname_final <- paste0("_", resname[res])
if(var=="solar") resname_final <- ""
# final file name:
file <- paste0(varname[var], resname_final, "_Beschreibung_Stationen.txt")
# ------------------------------------------------------------------------------
# File download
widths <- c( 6,9,9,15,12,10,41,100)
# read one line to confirm widths and get column names
oneline <- readLines(file, n=3)
if(substr(oneline[3],1,6)=="      ") widths[1] <- 11
# actually read metadata: suppress readLines warning about EOL
stats <- suppressWarnings(read.fwf(file, widths=widths, skip=2, strip.white=TRUE) )
# column names:
colnames(stats) <- strsplit(oneline[1], " ")[[1]]
# prepare file writing:
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
outfile <- tail(strsplit(file, "/")[[1]], 1)
outfile <- fileDWD(outfile)
# write file:
write.table(stats, file=outfile, row.names=FALSE, quote=FALSE, sep="\t")
# output:
return(stats)
}
