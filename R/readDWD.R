#' Process data from the DWD CDC FTP Server
#' 
#' Read climate data that was downloaded with \code{\link{dataDWD}}.
#' The file is read, processed and returned as a data.frame.\cr
#' New users are advised to set varnames=TRUE.\cr
#' \code{file} can be a vector with several filenames. The arguments \code{meta}
#' and \code{format} can also be a vector and will be recycled to the length of \code{file}.\cr
#' If \code{meta=TRUE}, column widths for \code{\link{read.fwf}} are computed internally.
#' If needed, readDWD tries to set the locale to German (to handle Umlaute correctly).
#' They can then be processed with \code{dd$Stations_id <- berryFunctions::convertUmlaut(dd$Stations_id)}.
#' 
#' @return Invisible data.frame of the desired dataset, 
#'         or a named list of data.frames if length(file) > 1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{readVars}}, \code{\link{readMeta}}, \code{\link{selectDWD}}
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf untar
#' @importFrom berryFunctions checkFile na9 traceCall l2df
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD
#' 
#' @param file   Char (vector): name(s) of the file(s) downloaded with \code{\link{dataDWD}},
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip" or
#'               "~/DWDdata/RR_Stundenwerte_Beschreibung_Stationen.txt"
#' @param meta   Logical (vector): is the \code{file} a meta file (Beschreibung.txt)?
#'               DEFAULT: TRUE for each file ending in ".txt"
#' @param binary Logical (vector): is the \code{file} a binary file, like for the 
#'               radolan grid files? The argument \code{selection} (DEFAULT: NULL)
#'               can be used to read only a subset of the ~24*31=744 files
#'               (called as \code{f[selection]}).
#'               DEFAULT: TRUE for each file ending in ".tar.gz"
#' @param raster Logical (vector): is the \code{file} a raster file, like for the 
#'               seasonal grid files? The argument \code{dividebyten} (DEFAULT: TRUE)
#'               can be used the devide the values by ten.
#'               DEFAULT: TRUE for each file ending in ".asc.gz"
#' @param multia Logical (vector): is the \code{file} a multi_annual file?
#'               Overrides meta, so set to FALSE manually if meta reading 
#'               needs to be called on a file ending with "Standort".
#'               DEFAULT: TRUE for each file ending in "Standort.txt"
#' @param fread  Logical: read faster with \code{data.table::\link[data.table]{fread}}?
#'               For 30 daily/kl/hist files, 7 instead of 10 seconds.
#'               NA can also be used, which means TRUE if data.table is available.
#'               Keep \code{varnames=FALSE} for the speed gain!
#'               DEFAULT: FALSE
#' @param varnames Logical (vector): add a short description to the DWD variable 
#'               abbreviations in the column names?
#'               E.g. change \code{FX,TNK} to \code{FX.Windspitze,TNK.Lufttemperatur_Min},
#'               see \code{link{newColumnNames}}.
#'               DEFAULT: FALSE (for backwards compatibility) 
#' @param format Char (vector), only used if \code{meta=FALSE}: Format passed to
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
#'               DEFAULT: TRUE
#' @param \dots  Further arguments passed to the underlying reading function, i.e.
#'               \code{\link{read.table}}, \code{data.table::\link[data.table]{fread}},
#'               \code{\link{read.fwf}}, \code{\link{readBin}} or
#'               \code{raster::\link[raster]{raster}}
#' 
readDWD <- function(
file,
meta=grepl('.txt$', file),
binary=grepl('.tar.gz$', file),
raster=grepl('.asc.gz$', file),
multia=grepl('Standort.txt$', file),
fread=FALSE,
varnames=FALSE,
format=NA,
tz="GMT",
progbar=TRUE,
...
)
{
# recycle meta, format and tz
len <- length(file)
if(missing(progbar) & len==1 & all(!binary)) progbar <- FALSE
if(anyNA(fread)) fread[is.na(fread)] <- requireNamespace("data.table",quietly=TRUE)
if(len>1)
  {
  meta   <- rep(meta,   length.out=len)
  binary <- rep(binary, length.out=len)
  raster <- rep(raster, length.out=len)
  multia <- rep(multia, length.out=len)
  fread  <- rep(fread,  length.out=len)
  varnames<-rep(varnames,length.out=len)
  format <- rep(format, length.out=len)
  tz     <- rep(tz,     length.out=len)
  }
meta[multia] <- FALSE
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
# check package availability:
if(any(fread))   if(!requireNamespace("data.table", quietly=TRUE))
    stop("in rdwd::readDWD: to use fread=TRUE, please first install data.table:",
         "   install.packages('data.table')", call.=FALSE)
#
checkFile(file)
# Handle German Umlaute:
if(any(meta))
{
lct <- Sys.getlocale("LC_CTYPE")
on.exit(Sys.setlocale(category="LC_CTYPE", locale=lct), add=TRUE)
if(!grepl(pattern="german", lct, ignore.case=TRUE))
  {
  lctry <- c("German","de_DE","de_DE.UTF-8","de_DE.utf8","de")
  for(lc in lctry) if(suppressWarnings(Sys.setlocale("LC_CTYPE", lc))!="") break
  }
}
#
#
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
f <- file[i]
# if meta/binary/raster:
if(meta[i])   return(readDWD.meta(f, ...))
if(binary[i]) return(readDWD.binary(f, ..., progbar=progbar))
if(raster[i]) return(readDWD.raster(f, ...))
if(multia[i]) return(readDWD.multia(f, ...))
# if data:
readDWD.data(f, fread=fread[i], varnames=varnames[i], format=format[i], tz=tz[i], ...)
}) # lapply loop end
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}




# internal code for each file type ----

readDWD.data <- function(file, fread=FALSE, varnames, format, tz, ...)
{
if(fread)
  {
  # http://dsnotes.com/post/2017-01-27-lessons-learned-from-outbrain-click-prediction-kaggle-competition/
  fp <- unzip(file, list=TRUE) # file produkt*, the actual datafile
  fp <- fp$Name[grepl("produkt",fp$Name)]
  dat <- data.table::fread(cmd=paste("unzip -p", file, fp), na.strings=na9(nspace=0),
                           header=TRUE, sep=";", stringsAsFactors=TRUE, data.table=FALSE, ...)
  if(varnames) dat <- newColumnNames(dat, readVars(file))
  } else
{
# temporary unzipping directory
fn <- tools::file_path_sans_ext(basename(file))
exdir <- paste0(tempdir(),"/", fn)
unzip(file, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE), add=TRUE)
# Read the actual data file:
f <- dir(exdir, pattern="produkt*", full.names=TRUE)
if(length(f)!=1) stop("There should be a single 'produkt*' file, but there are ",
                      length(f), " in\n  ", file, "\n  Consider re-downloading (with force=TRUE).")
dat <- read.table(f, na.strings=na9(), header=TRUE, sep=";", as.is=FALSE, ...)
if(varnames) 
  {
  vars <- dir(exdir, pattern="Metadaten_Parameter.*txt", full.names=TRUE)
  if(length(vars)!=1) warning("No Metadaten_Parameter.*txt file available in\n",
                              file)
  vars <- readVars.internal(vars, fn) # much quicker if already unzipped!
  dat <- newColumnNames(dat, vars)
  }
} # end if(!fread)
#
# process time-stamp: http://stackoverflow.com/a/13022441
if(!is.null(format))
  {
  # for res=monthly data:
  if("MESS_DATUM_BEGINN" %in% colnames(dat))
    dat <- cbind(dat[,1, drop=FALSE], MESS_DATUM=dat$MESS_DATUM_BEGINN + 15, dat[,-1])
  if(!"MESS_DATUM" %in% colnames(dat)) 
    warning("There is no column 'MESS_DATUM' in ",file, call.=FALSE) else
    {
    nch <- nchar(as.character(dat$MESS_DATUM[1]))
    if(is.na(format)) format <- if(nch== 8) "%Y%m%d" else 
                                if(nch==13) "%Y%m%d%H:%M" else"%Y%m%d%H"
    dat$MESS_DATUM <- as.POSIXct(as.character(dat$MESS_DATUM), format=format, tz=tz)
    }
  }
# final output:
return(dat)
}




readDWD.meta <- function(file, ...)
{
# read one line to get column widths and names
oneline <- readLines(file, n=3, encoding="latin1")
# column widths (automatic detection across different styles used by the DWD)
spaces <- unlist(gregexpr(" ", oneline[3]))
breaks <- spaces[which(diff(spaces)!=1)]
if(substr(oneline[3],1,1)==" ") breaks <- breaks[-1]
breaks[3] <- breaks[3] -9 # right-adjusted column
breaks[4:5] <- breaks[4:5] -1 # right-adjusted columns
widths <- diff(c(0,breaks,200))
sdsf <- grepl("subdaily_standard_format", file)
if(sdsf) widths <- c(6,6,9,10,10,10,10,26,200)
# actually read metadata, suppress readLines warning about EOL:
stats <- suppressWarnings(read.fwf(file, widths=widths, skip=2, strip.white=TRUE, 
                                   fileEncoding="latin1", ...) )
# column names:
# remove duplicate spaces (2018-03 only in subdaily_stand...Beschreibung....txt)
while( grepl("  ",oneline[1]) )  oneline[1] <- gsub("  ", " ", oneline[1])
colnames(stats) <- strsplit(oneline[1], " ")[[1]]
if(sdsf)
 {
 stats <- stats[ ! stats[,1] %in% c("","ST_KE","-----") , ]
 tf <- tempfile()
 write.table(stats[,-1], file=tf, quote=FALSE, sep="\t")
 stats <- read.table(tf, sep="\t")
 colnames(stats) <- c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", 
                      "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
 }
# check classes:
classes <- c("integer", "integer", "integer", "integer", "numeric", "numeric", "factor", "factor")
actual <- sapply(stats, class)
if(actual[4]=="numeric") classes[4] <- "numeric"
if(!all(actual == classes))
  {
  msg <- paste0(names(actual)[actual!=classes], ": ", actual[actual!=classes],
                " instead of ", classes[actual!=classes], ".")
  msg <- paste(msg, collapse=" ")
  warning(traceCall(3, "", ": "), "reading file '", file,
          "' did not give the correct column classes. ", msg, call.=FALSE)
  }
# return meta data.frame:
stats
}




readDWD.binary <- function(file, progbar=TRUE, selection=NULL, ...)
{
# temporary unzipping directory
fn <- tools::file_path_sans_ext(basename(file))
exdir <- paste0(tempdir(),"/", fn)
untar(file, exdir=exdir) # may take a few seconds
on.exit(unlink(exdir, recursive=TRUE), add=TRUE)

# hourly files;
f <- dir(exdir, full.names=TRUE) # 31*24 = 744 files  (daily/hist/2017-12)
if(!is.null(selection)) f <- f[selection]
# binary file header:   substr(readLines(f[1], n=1, warn=FALSE), 1, 270)
if(progbar) lapply <- pbapply::pblapply
# Read the actual binary file:
rb <- lapply(f, readBin, what="int", size=2, n=900*900, endian="little", ...)
# list element names (time stamp):
time <- l2df(strsplit(f,"-"))[,3]
time <- strptime(time, format="%y%m%d%H%M")
time <- format(time, "%Y-%m-%d_%H:%M")
names(rb) <- time
return(invisible(rb))
}




readDWD.raster <- function(file, dividebyten=TRUE, ...)
{
if(!requireNamespace("R.utils", quietly=TRUE))
  stop("To use rdwd:::readDWD.raster, please first install R.utils:",
       "   install.packages('R.utils')", call.=FALSE)
if(!requireNamespace("raster", quietly=TRUE))
 stop("To use rdwd:::readDWD.raster, please first install raster:",
      "   install.packages('raster')", call.=FALSE)
#https://stackoverflow.com/questions/5227444/recursively-ftp-download-then-extract-gz-files
rdata <- R.utils::gunzip(file, remove=FALSE, overwrite=TRUE)
r <- raster::raster(rdata, ...)
if(dividebyten) r <- r/10
return(invisible(r))
}



readDWD.multia <- function(file, ...)
{
out <- read.table(file, sep=";", header=TRUE, ...)
nc <- ncol(out)
# presumably, all files have a trailing empty column...
if(colnames(out)[nc]=="X") out <- out[,-nc]
out
}

