# read dwd ----

#' @title Process data from the DWD CDC FTP Server
#' @description Read climate data that was downloaded with \code{\link{dataDWD}}.
#' The data is unzipped and subsequently, the file is read, processed and
#' returned as a data.frame / raster object.\cr
#' New users are advised to set \code{varnames=TRUE} to obtain more informative
#' column names.\cr\cr
#' \code{readDWD} will call internal (but documented) functions depending on the
#' arguments \code{multia, meta, stand, binary, raster, nc, radar, asc}:\cr
#' to read observational data (\href{https://bookdown.org/brry/rdwd/available-datasets.html}{overview}): \code{\link{readDWD.data},
#'          \link{readDWD.multia}, \link{readDWD.stand}, \link{readDWD.meta}}\cr
#' to read gridded data (\href{https://bookdown.org/brry/rdwd/raster-data.html}{overview}): \code{\link{readDWD.binary},
#'          \link{readDWD.raster}, \link{readDWD.radar}, \link{readDWD.nc}, \link{readDWD.asc}}\cr
#' Not all arguments to \code{readDWD} are used for all functions, e.g. 
#' \code{fread} is used only by \code{.data}, while \code{dividebyten} 
#' is used in \code{.raster} and \code{.asc}.\cr\cr
#' \code{file} can be a vector with several filenames. Most other arguments can
#' also be a vector and will be recycled to the length of \code{file}.
#' 
#' @return Invisible data.frame of the desired dataset, 
#'         or a named list of data.frames if length(file) > 1.\cr
#'         The functions for gridded data return raster objects instead of data.frames.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016, Winter 2018/19
#' @seealso \code{\link{dataDWD}}, \code{\link{readVars}}, 
#'          \code{\link{readMeta}}, \code{\link{selectDWD}}\cr
#'          \url{https://bookdown.org/brry/rdwd}
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf untar write.table
#' @importFrom berryFunctions checkFile na9 traceCall l2df owa
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD
#' 
#' @param file   Char (vector): name(s) of the file(s) downloaded with 
#'               \code{\link{dataDWD}},
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip" or
#'               "~/DWDdata/RR_Stundenwerte_Beschreibung_Stationen.txt"
#' @param quiet  Logical: suppress messages? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: !quiet
#' @param fread  Logical (vector): read fast? See \code{\link{readDWD.data}}.
#'               DEFAULT: FALSE (some users complain it doesn't work on their PC)
#' @param varnames Logical (vector): Expand column names? 
#'               See \code{\link{readDWD.data}}. DEFAULT: FALSE
#' @param var    var for \code{\link{readDWD.nc}}. DEFAULT: ""
#' @param format,tz Format and time zone of time stamps, see \code{\link{readDWD.data}}
#' @param dividebyten Logical (vector): Divide the values in raster files by ten?
#'               Used in \code{\link{readDWD.raster}} and \code{\link{readDWD.asc}}.
#'               DEFAULT: TRUE
#' @param multia Logical (vector): is the \code{file} a multi_annual file?
#'               Overrides \code{meta}, so set to FALSE manually if 
#'               \code{\link{readDWD.meta}} needs to be called on a (manually renamed) 
#'               Beschreibung file ending with "Standort.txt". 
#'               See \code{\link{readDWD.multia}}.
#'               DEFAULT: TRUE for each file ending in "Standort.txt"
#' @param stand  Logical (vector): is the \code{file} a subdaily/standard_format file?
#'               See \code{\link{readDWD.stand}}.
#'               DEFAULT: TRUE fo files containing "standard_format" in the name.
#' @param meta   Logical (vector): is the \code{file} a meta file (Beschreibung.txt)?
#'               See \code{\link{readDWD.meta}}.
#'               For zip files containing station meta information, see 
#'               \code{\link{readMeta}}.
#'               DEFAULT: TRUE for each file ending in ".txt"
#' @param binary Logical (vector): does the \code{file} contain binary files?
#'               See \code{\link{readDWD.binary}}.
#'               DEFAULT: TRUE for each file ending in ".tar.gz"
#' @param raster Logical (vector): does the \code{file} contain a raster file?
#'               See \code{\link{readDWD.raster}}.
#'               DEFAULT: TRUE for each file ending in ".asc.gz"
#' @param nc     Logical (vector): does the \code{file} contain a netcdf file?
#'               See \code{\link{readDWD.nc}}.
#'               DEFAULT: TRUE for each file ending in ".nc.gz"
#' @param radar  Logical (vector): does the \code{file} contain a single binary file?
#'               See \code{\link{readDWD.radar}}.
#'               DEFAULT: TRUE for each file ending in ".gz"
#' @param asc    Logical (vector): does the \code{file} contain asc files?
#'               See \code{\link{readDWD.asc}}.
#'               DEFAULT: TRUE for each file ending in ".tar"
#' @param \dots  Further arguments passed to the internal \code{readDWD.*} 
#'               functions and from those to the underlying reading functions
#'               documented in each internal function.
#' 
readDWD <- function(
file,
quiet=rdwdquiet(),
progbar=!quiet,
fread=FALSE,
varnames=FALSE,
var="",
format=NA,
tz="GMT",
dividebyten=TRUE,
multia=grepl(  'Standort.txt$', file),
meta=  grepl(          '.txt$', file),
stand= grepl("standard_format", file),
binary=grepl(       '.tar.gz$', file),
raster=grepl(       '.asc.gz$', file),
nc=    grepl(        '.nc.gz$', file),
radar =grepl(           '.gz$', file),
asc=   grepl(          '.tar$', file),
...
)
{
# recycle arguments:
len <- length(file)
if(missing(progbar) & len==1 & all(!binary) & all(!asc)) progbar <- FALSE
if(anyNA(fread)) fread[is.na(fread)] <- requireNamespace("data.table",quietly=TRUE)
if(len>1)
  {
  fread       <- rep(fread,       length.out=len)
  varnames    <- rep(varnames,    length.out=len)  
  var         <- rep(var,         length.out=len)
  format      <- rep(format,      length.out=len)
  tz          <- rep(tz,          length.out=len)
  dividebyten <- rep(dividebyten, length.out=len)
  multia      <- rep(multia,      length.out=len)
  stand       <- rep(stand,       length.out=len)
  meta        <- rep(meta,        length.out=len)
  binary      <- rep(binary,      length.out=len)
  raster      <- rep(raster,      length.out=len)
  nc          <- rep(nc,          length.out=len)
  radar       <- rep(radar,       length.out=len)
  asc         <- rep(asc,         length.out=len) 
  }
meta[multia] <- FALSE
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
# check package availability:
if(any(fread))  checkSuggestedPackage("data.table", "rdwd::readDWD with fread=TRUE") 
#
checkFile(file)
# Handle German Umlaute:
if(any(meta)) # faster to change locale once here, instead of in each readDWD.meta call
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
if(progbar) message("Reading ", length(file), " file", if(length(file)>1)"s", "...")
#
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
# if meta/multia/radar/binary/raster/asc:
if(multia[i]) return(readDWD.multia(file[i], quiet=quiet, ...))
if(stand[i])  return(readDWD.stand( file[i], quiet=quiet, ...))
if(meta[i])   return(readDWD.meta(  file[i], quiet=quiet, ...))
if(binary[i]) return(readDWD.binary(file[i], quiet=quiet, progbar=progbar, ...))
if(raster[i]) return(readDWD.raster(file[i], dividebyten=dividebyten[i], quiet=quiet, ...))
if(nc[i])     return(readDWD.nc(    file[i], var=var[i], quiet=quiet, ...))
if(radar[i])  return(readDWD.radar( file[i], quiet=quiet, ...))
if(asc[i])    return(readDWD.asc(   file[i], quiet=quiet, progbar=progbar, dividebyten=dividebyten[i], ...))
# if data:
readDWD.data(file[i], fread=fread[i], varnames=varnames[i], 
             format=format[i], tz=tz[i], quiet=quiet, ...)
}) # lapply loop end
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}





# read observational data ----

# ~ data ----

#' @title read regular dwd data
#' @description Read regular dwd data. 
#' Intended to be called via \code{\link{readDWD}}.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso \code{\link{readDWD}}, Examples in \code{\link{dataDWD}}
#' @param file     Name of file on harddrive, like e.g. 
#'                 DWDdata/daily_kl_recent_tageswerte_KL_03987_akt.zip
#' @param fread    Logical: read faster with \code{data.table::\link[data.table]{fread}}?
#'                 When reading many large historical files, speedup is significant.
#'                 NA can also be used, which means TRUE if data.table is available.
#'                 DEFAULT: FALSE
#' @param varnames Logical (vector): add a short description to the DWD variable 
#'                 abbreviations in the column names?
#'                 E.g. change \code{FX,TNK} to \code{FX.Windspitze,TNK.Lufttemperatur_Min},
#'                 see \code{\link{newColumnNames}}.
#'                 DEFAULT: FALSE (for backwards compatibility) 
#' @param format   Char (vector): Format passed to
#'                 \code{\link{as.POSIXct}} (see \code{\link{strptime}})
#'                 to convert the date/time column to POSIX time format.\cr
#'                 If NULL, no conversion is performed (date stays a factor).
#'                 If NA, \code{readDWD} tries to find a suitable format based
#'                 on the number of characters. DEFAULT: NA
#' @param tz       Char (vector): time zone for \code{\link{as.POSIXct}}.
#'                 "" is the current time zone, and "GMT" is UTC (Universal Time,
#'                 Coordinated). DEFAULT: "GMT"
#' @param quiet    Suppress empty file warnings and subfuntion name message? 
#'                 DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots    Further arguments passed to \code{\link{read.table}} or 
#'                 \code{data.table::\link[data.table]{fread}}
readDWD.data <- function(file, fread=FALSE, varnames=FALSE, format=NA, tz="GMT", 
                         quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.data().")
if(fread)
  {
  # http://dsnotes.com/post/2017-01-27-lessons-learned-from-outbrain-click-prediction-kaggle-competition/
  fp <- unzip(file, list=TRUE) # file produkt*, the actual datafile
  fp <- fp$Name[grepl("produkt",fp$Name)]
  dat <- data.table::fread(cmd=paste("unzip -p", file, fp), na.strings=na9(nspace=0),
                           header=TRUE, sep=";", stringsAsFactors=TRUE, data.table=FALSE, ...)
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
} # end if(!fread)
#
if(varnames)  dat <- newColumnNames(dat)
# return if file is empty, e.g. for daily/more_precip/hist_05988 2019-05-16:
if(nrow(dat)==0)
  {
  if(!quiet) warning("File contains no rows: ", file)
  return(dat)
  }
# process time-stamp: http://stackoverflow.com/a/13022441
if(!is.null(format))
  {
  # for res=monthly data:
  if("MESS_DATUM_BEGINN" %in% colnames(dat))
    dat <- cbind(dat[,1, drop=FALSE], MESS_DATUM=dat$MESS_DATUM_BEGINN + 14, dat[,-1])
  if(!"MESS_DATUM" %in% colnames(dat)) 
    warning("There is no column 'MESS_DATUM' in ",file, call.=FALSE) else
    {
    nch <- nchar(as.character(dat$MESS_DATUM[1]))
    if(is.na(format)) format <- if(nch== 8) "%Y%m%d" else 
                                if(nch==12) "%Y%m%d%H%M" else # for 201804270020 10min data 
                                if(nch==13) "%Y%m%d%H:%M" else"%Y%m%d%H"
    dat$MESS_DATUM <- as.POSIXct(as.character(dat$MESS_DATUM), format=format, tz=tz)
    }
  }
# final output:
return(dat)
}



# ~ multia ----

#' @title read multi_annual dwd data
#' @description read multi_annual dwd data. 
#' Intended to be called via \code{\link{readDWD}}.\cr
#' All other observational data at \code{\link{dwdbase}} can be read
#' with \code{\link{readDWD.data}}, except for the multi_annual and 
#' subdaily/standard_format data.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2019
#' @seealso \code{\link{readDWD}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' # Temperature aggregates (2019-04 the 9th file):
#' durl <- selectDWD(res="multi_annual", var="mean_81-10", per="")[9]
#' murl <- selectDWD(res="multi_annual", var="mean_81-10", per="", meta=TRUE)[9]
#' 
#' ma_temp <- dataDWD(durl, dir=localtestdir())
#' ma_meta <- dataDWD(murl, dir=localtestdir())
#' 
#' head(ma_temp)
#' head(ma_meta)
#' 
#' ma <- merge(ma_meta, ma_temp, all=TRUE)
#' berryFunctions::linReg(ma$Stationshoehe, ma$Jahr)
#' op <- par(mfrow=c(3,4), mar=c(0.1,2,2,0), mgp=c(3,0.6,0))
#' for(m in colnames(ma)[8:19])
#'   {
#'   berryFunctions::linReg(ma$Stationshoehe, ma[,m], xaxt="n", xlab="", ylab="", main=m)
#'   abline(h=0)
#'   }
#' par(op)
#' 
#' par(bg=8)
#' berryFunctions::colPoints(ma$geogr..Laenge, ma$geogr..Breite, ma$Jahr, add=F, asp=1.4)
#' 
#' data("DEU")
#' pdf("MultiAnn.pdf", width=8, height=10)
#' par(bg=8)
#' for(m in colnames(ma)[8:19])
#'   {
#'   raster::plot(DEU, border="darkgrey")
#'   berryFunctions::colPoints(ma[-262,]$geogr..Laenge, ma[-262,]$geogr..Breite, ma[-262,m], 
#'                             asp=1.4, # Range=range(ma[-262,8:19]), 
#'                             col=berryFunctions::divPal(200, rev=TRUE), zlab=m, add=T)
#'   }
#' dev.off()
#' berryFunctions::openFile("MultiAnn.pdf")
#' }
#' @param file  Name of file on harddrive, like e.g. 
#'              DWDdata/multi_annual_mean_81-10_Temperatur_1981-2010_aktStandort.txt or
#'              DWDdata/multi_annual_mean_81-10_Temperatur_1981-2010_Stationsliste_aktStandort.txt
#' @param fileEncoding \link{read.table} \link{file} encoding.
#'              DEFAULT: "latin1" (needed on Linux, optional but not hurting on windows)
#' @param comment.char \link{read.table} comment character.
#'              DEFAULT: "\\032" (needed 2019-04 to ignore the binary 
#'              control character at the end of multi_annual files)
#' @param quiet Suppress subfunction name message? 
#'              DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots Further arguments passed to \code{\link{read.table}}
readDWD.multia <- function(file, fileEncoding="latin1", comment.char="\032", 
                           quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.multia().")
out <- read.table(file, sep=";", header=TRUE, fileEncoding=fileEncoding, 
                  comment.char=comment.char, ...)
nc <- ncol(out)
# presumably, all files have a trailing empty column...
if(colnames(out)[nc]=="X") out <- out[,-nc]
out
}



# ~ stand ----

#' @title read subdaily/standard_format dwd data
#' @description read subdaily/standard_format dwd data. 
#' Intended to be called via \code{\link{readDWD}}.\cr
#' All other observational data at \code{\link{dwdbase}} can be read
#' with \code{\link{readDWD.data}}, except for the multi_annual and 
#' subdaily/standard_format data.
#' @return data.frame with column names as per \code{\link{formatIndex}}.
#' "Q"-columns have "_parameter" appended to their name. A "Date" column has been added.
#' NA-indicators have been processed into NAs.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2019
#' @seealso \code{\link{readDWD}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' link <- selectDWD(id=10381, res="subdaily", var="standard_format", per="r")
#' file <- dataDWD(link, dir=localtestdir(), read=FALSE)
#' sf <- readDWD(file)
#' 
#' sf2 <- readDWD(file, fast=FALSE) # 20 secs!
#' stopifnot(all.equal(sf, sf2))
#' 
#' plot(sf$Date, sf$SHK, type="l")
#' 
#' # Plot all columns:
#' if(FALSE){ # not run in any automated testing
#' tmp <- tempfile(fileext=".pdf")
#' char2fact <- function(x) 
#'  {
#'  if(all(is.na(x))) return(rep(-9, len=length(x)))
#'  if(!is.numeric(x)) as.factor(x) else x
#'  }
#' pdf(tmp, width=9)
#' par(mfrow=c(2,1),mar=c(2,3,2,0.1), mgp=c(3,0.7,0), las=1)
#' for(i in 3:ncol(sf)-1) plot(sf$Date, char2fact(sf[,i]), type="l", main=colnames(sf)[i], ylab="")
#' dev.off()
#' berryFunctions::openFile(tmp)
#' }
#' }
#' @param file  Name of file on harddrive, like e.g. 
#'              DWDdata/subdaily_standard_format_kl_10381_00_akt.txt or
#'              DWDdata/subdaily_standard_format_kl_10381_bis_1999.txt.gz
#' @param fast  Logical: use \code{readr::\link[readr]{read_fwf}} 
#'              instead of \code{\link{read.fwf}}? 
#'              Takes 0.1 instead of 20 seconds but requires package to be installed.
#'              if fast=TRUE, \code{fileEncoding} is ignored.
#'              DEFAULT: TRUE
#' @param fileEncoding \link{read.table} \link{file} encoding.
#'              DEFAULT: "latin1" (potentially needed on Linux, 
#'              optional but not hurting on windows)
#' @param formIndex Single object: Index used to select column widts and NA values.
#'              To use a current / custom index, see the source code of
#'              \code{\link{updateIndexes}} at
#'              \url{https://github.com/brry/rdwd/blob/master/R/updateIndexes.R}.
#'              DEFAULT: \code{rdwd:::\link{formatIndex}}
#' @param quiet Suppress subfunction name message? 
#'              DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots Further arguments passed to \code{\link{read.fwf}}
#'              or \code{readr::\link[readr]{read_fwf}} 
readDWD.stand <- function(file, fast=TRUE, fileEncoding="latin1", 
                          formIndex=formatIndex, quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.stand().")
# check column existence
musthave <- c("Pos","Fehlk","dividebyten","Label")
has <- musthave %in% colnames(formIndex)
if(any(!has)) stop("formIndex must contain column(s) ", musthave[!has])
# get column widths:
width <- diff(as.numeric(formIndex$Pos))
width <- c(width, 1)
# read fixed width dataset:
if(fast)
  {
  checkSuggestedPackage("readr", "readDWD.stand with fast=TRUE")  
  coltypes <- readr::cols(X22=readr::col_character()) # 22 is S column
  # if not specified, will guess logical and then complain about character input later in file
  sf <- readr::read_fwf(file, readr::fwf_widths(width), col_types=coltypes, ...)
  sf <- data.frame(sf) # loose tibble attributes and printing methods
  # mimick read.fwf behaviour:
  sf[is.na(sf)] <- " " # to avoid NA comparison
  for(i in c(2,4,5,6)) sf[,i] <- as.integer(sf[,i])
  } else # see developmentNotes for speed comparison
  sf <- read.fwf(file, widths=width, stringsAsFactors=FALSE, fileEncoding=fileEncoding, ...) 
# dimension check:
if(ncol(sf) != nrow(formIndex)) stop("incorrectly read file: ", file,"\n", 
   ncol(sf), " columns instead of ", nrow(formIndex), " as dictated by formIndex.")
# NAs (starting with column 7):
for(i in which(formIndex$Fehlk!=""))
  {
  isNA <- as.character(sf[,i])==formIndex$Fehlk[i]
  if(anyNA(isNA)) stop("NAs in comparison in column ", i, " of file ", file)
  sf[isNA, i] <- NA
  }
# divide by ten:
for(i in which(formIndex$dividebyten)) sf[,i] <- sf[,i]/10
# column names:
cn <- formIndex$Label
Qind <- which(cn=="Q")
cn[Qind] <- paste0(cn[Qind], "_", cn[Qind-1])
cn[cn==""] <- "Leer"
colnames(sf) <- cn
# add Date column:
sf$Date <- as.Date(paste(sf$JA,sf$MO,sf$TA,sep="-"), "%F")
#output:
return(sf)
}



# ~ meta ----

#' @title read dwd metadata (Beschreibung*.txt files)
#' @description read dwd metadata (Beschreibung*.txt files).
#'  Intended to be called via \code{\link{readDWD}}.\cr
#'  Column widths for \code{\link{read.fwf}} are computed internally.\cr
#'  if(any(meta)), \code{\link{readDWD}} tries to set the locale to German 
#'  (to handle Umlaute correctly). It is hence not recommended to call
#'  \code{rdwd:::readDWD.meta} directly on a file!\cr
#'  Names can later be changed to ascii with 
#'  \code{berryFunctions::\link{convertUmlaut}}.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso \code{\link{readDWD}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' link <- selectDWD(res="daily", var="kl", per="r", meta=TRUE)
#' if(length(link)!=1) stop("length of link should be 1, but is ", length(link), 
#'                 ":\n", berryFunctions::truncMessage(link,prefix="",sep="\n"))
#' 
#' file <- dataDWD(link, dir=localtestdir(), read=FALSE)
#' meta <- readDWD(file)
#' head(meta)
#' 
#' cnm <- colnames(meta)
#' if(length(cnm)!=8) stop("number of columns should be 8, but is ", length(cnm),
#'                         ":\n", toString(cnm))
#' }
#' @param file  Name of file on harddrive, like e.g. 
#'              DWDdata/daily_kl_recent_KL_Tageswerte_Beschreibung_Stationen.txt
#' @param quiet Suppress subfunction name message? 
#'              DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots Further arguments passed to \code{\link{read.fwf}}
readDWD.meta <- function(file, quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.meta().")
# read a few lines to get column widths and names
oneline <- readLines(file, n=60, encoding="latin1") 
# n=60 or 15 has no influence on total readDWD.meta time for 97 meta files (16 secs)

# column names:
# remove duplicate spaces (2018-03 only in subdaily_stand...Beschreibung....txt)
while( grepl("  ",oneline[1]) )  oneline[1] <- gsub("  ", " ", oneline[1])
cnames <- strsplit(oneline[1], " ")[[1]]
choehe <- grep("hoehe", cnames, ignore.case=TRUE) - 1

# column widths (automatic detection across different styles used by the DWD)
spaces <- Reduce(intersect, gregexpr(" ", oneline[-(1:2)]) )
breaks <- spaces[which(diff(spaces)!=1)]
breaks[choehe] <- breaks[choehe] - 3 #  to capture 4-digit Stationshoehen (1134 m Brocken, eg)
widths <- diff(c(0,breaks,200))
#
# actually read metadata, suppress readLines warning about EOL:
stats <- suppressWarnings(read.fwf(file, widths=widths, skip=2, strip.white=TRUE, 
                                   fileEncoding="latin1", ...) )
colnames(stats) <- cnames
if(grepl("subdaily_standard_format", file))
 {
 stats <- stats[ ! stats[,1] %in% c("","ST_KE","-----") , ]
 tf <- tempfile()
 write.table(stats[,-1], file=tf, quote=FALSE, sep="\t")
 stats <- read.table(tf, sep="\t")
 colnames(stats) <- c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", 
                      "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
 }
# check classes:
if(ncol(stats)!=8) stop(ncol(stats)," columns detected instead of 8 for ", file)
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



# read gridded data ----

# ~ binary ----

#' @title read dwd gridded radolan binary data
#' @description read gridded radolan binary data.
#' Intended to be called via \code{\link{readDWD}}.\cr
#' @return list depending on argument \code{toraster}, see there for details
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018. 
#'         Significant input for the underlying \code{dwdradar::\link[dwdradar]{readRadarFile}} came
#'         from Henning Rust & Christoph Ritschel at FU Berlin.
#' @seealso \code{\link{readDWD}}, especially \code{\link{readDWD.radar}}\cr
#'   \url{https://wradlib.org} for much more extensive radar analysis in Python\cr
#'   Kompositformatbeschreibung at \url{https://www.dwd.de/DE/leistungen/radolan/radolan.html}
#'   for format description
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' # SF file as example: ----
#' 
#' SF_link <- "/daily/radolan/historical/bin/2017/SF201712.tar.gz"
#' SF_file <- dataDWD(file=SF_link, base=gridbase, joinbf=TRUE,   # 204 MB
#'                      dir=localtestdir(), read=FALSE)
#' # exdir radardir set to speed up my tests:
#' SF_exdir <- "C:/Users/berry/Desktop/DWDbinarySF"
#' if(!file.exists(SF_exdir)) SF_exdir <- tempdir()
#' # no need to read all 24*31=744 files, so setting selection:
#' SF_rad <- readDWD(SF_file, selection=1:10, exdir=SF_exdir) #with toraster=TRUE 
#' if(length(SF_rad)!=2) stop("length(SF_rad) should be 2, but is ", length(SF_rad))
#' 
#' SF_radp <- projectRasterDWD(SF_rad$dat)
#' raster::plot(SF_radp[[1]], main=SF_rad$meta$date[1])
#' addBorders()
#' 
#' 
#' # RW file as example: ----
#' 
#' RW_link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"
#' RW_file <- dataDWD(file=RW_link, base=gridbase, joinbf=TRUE,   # 25 MB
#'                   dir=localtestdir(), read=FALSE)
#' RW_exdir <- "C:/Users/berry/Desktop/DWDbinaryRW"
#' if(!file.exists(RW_exdir)) RW_exdir <- tempdir()
#' RW_rad <- readDWD(RW_file, selection=1:10, exdir=RW_exdir)
#' RW_radp <- projectRasterDWD(RW_rad$dat, extent="rw")
#' raster::plot(RW_radp[[1]], main=RW_rad$meta$date[1])
#' addBorders()
#' 
#' # ToDo: why are values + patterns not the same?
#' 
#' # list of all Files: ----
#' data(gridIndex)
#' head(grep("historical", gridIndex, value=TRUE))
#' }
#' @param file      Name of file on harddrive, like e.g. 
#'                  DWDdata/daily_radolan_historical_bin_2017_SF201712.tar.gz
#' @param exdir     Directory to unzip into. If existing, only the needed files
#'                  will be unpacked with \code{\link{untar}}. Note that exdir
#'                  size will be around 1.1 GB. exdir can contain other files, 
#'                  these will be ignored for the actual reading with 
#'                  \code{dwdradar::\link[dwdradar]{readRadarFile}}.
#'                  DEFAULT exdir: sub(".tar.gz$", "", file)
#' @param toraster  Logical: convert output (list of matrixes + meta informations)
#'                  to a list with dat (\code{raster \link[raster]{stack}}) + 
#'                  meta (list from the first subfile, but with vector of dates)?
#'                  DEFAULT: TRUE
#' @param quiet     Suppress progress messages? 
#'                  DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param progbar   Show progress bars? \code{\link{readDWD}} will
#'                  keep progbar=TRUE for binary files, even if length(file)==1.
#'                  DEFAULT: !quiet, i.e. TRUE
#' @param selection Optionally read only a subset of the ~24*31=744 files.
#'                  Called as \code{f[selection]}. DEFAULT: NULL (ignored)
#' @param \dots     Further arguments passed to \code{dwdradar::\link[dwdradar]{readRadarFile}}, 
#'                  i.e. \code{na} and \code{clutter}
readDWD.binary <- function(file, exdir=sub(".tar.gz$", "", file), toraster=TRUE, 
                           quiet=rdwdquiet(), progbar=!quiet, selection=NULL, ...)
{
checkSuggestedPackage("dwdradar", "rdwd:::readDWD.binary")
pmessage <- function(...) if(!quiet) message(...)
pmessage("Reading file with readDWD.binary().")
# Untar as needed:
pmessage("\nChecking which files need to be untarred to ", exdir, "...")
f <- sort(untar(file, list=TRUE))
if(!is.null(selection)) f <- f[selection]
tountar <- !f %in% dir(exdir)
if(any(tountar)) 
  {
  pmessage("Unpacking ",sum(tountar), " of ",length(f), " files from ",file,"...")
  ufiles <- if(any(!tountar) | !is.null(selection)) f[tountar] else NULL
  # warning message if all(tountar) for non-empty exdir
  # cannot read the message because of max options(warning.length) 8170
  # but this workaround does the job, so whatever.
  untar(file, files=ufiles, exdir=exdir)
  } else 
  pmessage("All files were already untarred.")
#
# hourly files:
fd <- dir(exdir, full.names=TRUE) # 31*24 = 744 files  (daily/hist/2017-12)
# read only the ones from file, not other stuff at exdir:
f <- fd[basename(fd) %in% f]
#
pmessage("Reading ",length(f)," binary files...")
if(progbar) lapply <- pbapply::pblapply
# Read the actual binary file:
rb <- lapply(f, dwdradar::readRadarFile, ...)
# list element names (time stamp):
time <- sapply(rb, function(x) as.character(x$meta$date))
names(rb) <- time
if(!toraster) return(invisible(rb))
# else if toraster:
checkSuggestedPackage("raster", "rdwd:::readDWD.binary with toraster=TRUE")
pmessage("Converting to raster...")
rbmat <- base::lapply(rb,"[[",1)
rbmat <- lapply(rbmat, raster::raster)
rbmat <- raster::stack(rbmat)
# rbmeta <- base::lapply(rb,"[[",2)
# rbmeta <- base::lapply(rbmeta, function(x){x$radars <- toString(x$radars);
#                                            x$radarn <- toString(x$radarn);
#                                            x$dim    <- toString(x$dim)   ; x})
# mnames <- names(rbmeta[[1]])[-(1:2)] # filename and date will differ
# sapply(mnames, function(mn) length(unique(sapply(rbmeta, "[[", mn)))) # all equal
rbmeta <- rb[[1]]$meta
rbmeta$filename <- file
rbmeta$date <- as.POSIXct(time)
return(invisible(list(dat=rbmat, meta=rbmeta)))
}




# ~ raster ----

#' @title read dwd gridded raster data
#' @description Read gridded raster data. 
#' Intended to be called via \code{\link{readDWD}}.\cr
#' Note that \code{R.utils} must be installed to unzip the .asc.gz files.
#' @return \code{raster::\link[raster]{raster}} object
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018
#' @seealso \code{\link{readDWD}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' rasterbase <- paste0(gridbase,"/seasonal/air_temperature_mean")
#' ftp.files <- indexFTP("/16_DJF", base=rasterbase, dir=tempdir())
#' localfiles <- dataDWD(ftp.files[1:2], base=rasterbase, joinbf=TRUE,
#'                       dir=localtestdir(), read=FALSE)
#' rf <- readDWD(localfiles[1])
#' rf <- readDWD(localfiles[1]) # runs faster at second time due to skip=TRUE
#' raster::plot(rf)
#' 
#' rfp <- projectRasterDWD(rf, proj="seasonal", extent=rf@extent)
#' raster::plot(rfp)
#' addBorders()
#' 
#' testthat::expect_equal(raster::cellStats(rf, range), c(-8.2,4.4))
#' rf10 <- readDWD(localfiles[1], dividebyten=FALSE)
#' raster::plot(rf10)
#' testthat::expect_equal(raster::cellStats(rf10, range), c(-82,44))
#' }
#' @param file        Name of file on harddrive, like e.g. 
#'                    DWDdata/grids_germany/seasonal/air_temperature_mean/
#'                    16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' @param gargs       Named list of arguments passed to 
#'                    \code{R.utils::\link[R.utils:compressFile]{gunzip}}. The internal 
#'                    defaults are: \code{remove=FALSE} (recommended to keep this
#'                    so \code{file} does not get deleted) and \code{skip=TRUE}
#'                    (which reads previously unzipped files as is).
#'                    If \code{file} has changed, you might want to use 
#'                    \code{gargs=list(skip=FALSE, overwrite=TRUE)}
#'                    or alternatively \code{gargs=list(temporary=TRUE)}.
#'                    The \code{gunzip} default \code{destname} means that the 
#'                    unzipped file is stored at the same path as \code{file}.
#'                    DEFAULT gargs: NULL
#' @param dividebyten Logical: Divide the numerical values by 10?
#'                    DEFAULT: TRUE
#' @param quiet       Suppress subfunction name message? 
#'                    DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots       Further arguments passed to \code{raster::\link[raster]{raster}}
readDWD.raster <- function(file, gargs=NULL, dividebyten, quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.raster().")
checkSuggestedPackage("R.utils", "rdwd:::readDWD.raster")
checkSuggestedPackage("raster",  "rdwd:::readDWD.raster")
#https://stackoverflow.com/questions/5227444/recursively-ftp-download-then-extract-gz-files
# gunzip arguments:
gdef <- list(filename=file, remove=FALSE, skip=TRUE)
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
rdata <- do.call(R.utils::gunzip, gfinal)
# raster reading:
r <- raster::raster(rdata, ...)
if(dividebyten) r <- r/10
return(invisible(r))
}



# ~ nc ----

#' @title read dwd netcdf data
#' @description Read netcdf data. 
#' Intended to be called via \code{\link{readDWD}}.\cr
#' Note that \code{R.utils} and \code{ncdf4} must be installed to unzip and read the .nc.gz files.
#' @return \code{raster::\link[raster]{brick}} object. Alternatively, 
#' if toraster=FALSE, a list with time, lat, lon, var, varname, file and cdf.
#' \bold{cdf} is the output of \code{ncdf4::\link[ncdf4]{nc_open}}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso \code{\link{readDWD}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' library(berryFunctions) # for seqPal and colPointsLegend
#' 
#' url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
#' url <- "daily/Project_TRY/humidity/RH_199509_daymean.nc.gz"    # 25 MB
#' file <- dataDWD(url, base=gridbase, joinbf=TRUE, dir=localtestdir(), read=FALSE)
#' nc <- readDWD(file)
#' ncp <- projectRasterDWD(nc, proj="nc", extent="nc")
#'  for(i in 1:3) raster::plot(ncp[[i]], col=seqPal(), 
#'                             main=paste(nc@title, nc@z[[1]][i]))
#' addBorders()
#' str(nc, max.level=2)
#' 
#' raster::values(nc[[1]]) # obtain actual values into memory
#'  
#' raster::plot(nc[[1]]) # axes 0:938 / 0:720, the number of grid cells
#' raster::plot(ncp[[1]])# properly projected, per default onto latlon
#' 
#' rng <- range(raster::cellStats(nc[[1:6]], "range"))
#' raster::plot(nc, col=seqPal(), zlim=rng, maxnl=6)
#' 
#' # Array instead of raster brick:
#' nc <- readDWD(file, toraster=FALSE)
#' image(nc$var[,,1], col=seqPal(), asp=1.1)
#' colPointsLegend(nc$var[,,1], title=paste(nc$varname, nc$time[1]))
#' 
#' # interactive selection of variable:
#' # nc <- readDWD(file, var="-") # uncommented to not block automated tests 
#' str(nc$var)
#' }
#' @param file        Name of file on harddrive, like e.g. 
#'                    DWDdata/grids_germany/daily/Project_TRY/humidity/RH_199509_daymean.nc.gz
#' @param gargs       Named list of arguments passed to 
#'                    \code{R.utils::\link[R.utils:compressFile]{gunzip}}, 
#'                    see \code{\link{readDWD.raster}}. DEFAULT: NULL
#' @param toraster    Read file with \code{raster::\link[raster]{brick}}? 
#'                    All further arguments will be ignored. Specify e.g. var through \dots.
#'                    DEFAULT: TRUE
#' @param var         if toraster=FALSE: Charstring with name of variable to be read with 
#'                    \code{ncdf4::\link[ncdf4]{ncvar_get}}. If not available, 
#'                    an interactive selection is presented.
#'                    DEFAULT: "" (last variable)
#' @param quiet       Logical: Suppress Suppress subfunction name message and 
#'                    time conversion failure warning? 
#'                    DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots       Further arguments passed to \code{raster::\link[raster]{brick}}
#'                    or \code{ncdf4::\link[ncdf4]{nc_open}}
#' 
readDWD.nc <- function(file, gargs=NULL, var="", toraster=TRUE, quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.nc().")
checkSuggestedPackage("ncdf4", "rdwd:::readDWD.nc") # also needed if toraster=TRUE
checkSuggestedPackage("R.utils", "rdwd:::readDWD.nc")
# gunzip arguments:
gdef <- list(filename=file, remove=FALSE, skip=TRUE)
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
ncfile <- do.call(R.utils::gunzip, gfinal)
#
if(toraster) 
  {
  checkSuggestedPackage("raster", "rdwd:::readDWD.nc with toraster=TRUE")
  return(invisible(raster::brick(ncfile, ...)))
  }
#
#
# NCDF File
mycdf <- ncdf4::nc_open(ncfile, ...)
#
# Time stamp:
unit <- mycdf$dim$time$units
if(is.null(unit))
 {
 if(!quiet) message("Time could not be read from cdf file and is set to 1.")
 time <- 1
 } else
if(substr(unit,1,4)=="days")
 {
 start <- sub("days since", "", unit)
 start <- strsplit(removeSpace(start), " ")[[1]][1]
 start <- as.Date(start)
 time <- start + ncdf4::ncvar_get(mycdf,'time')
 } else
 {
 start <- removeSpace(sub("hours since", "", unit)) # always hours?
 start <- strptime(start, format="%F %T")
 time <- start + ncdf4::ncvar_get(mycdf,'time')*3600
 }
#
# Var name
# select one of the possible variable names if not given:
varnames <- names(mycdf$var)
if(var=="") var <- tail(varnames, 1)
while(! var %in% varnames)
{
 pos <- varnames[!varnames %in% c("lon","lat")] # pos: possible
 if(length(pos)==1) var <- pos else
 var <- pos[menu(pos, title="Which variable do you want?")]
}
# Actually extract the desired variables:
LAT <- ncdf4::ncvar_get(mycdf, "lat")
LON <- ncdf4::ncvar_get(mycdf, "lon")
VAR <- ncdf4::ncvar_get(mycdf, var)
# output:
return(invisible(list(time=time, lat=LAT, lon=LON, var=VAR, varname=var, 
                      file=mycdf$filename, cdf=mycdf)))
}



# ~ radar ----

#' @title read dwd gridded radolan radar data
#' @description read gridded radolan radar data.
#' Intended to be called via \code{\link{readDWD}}.\cr
#' @return Invisible list with \code{dat} (matrix or raster, depending on \code{toraster}) 
#' and \code{meta} (list with elements from header)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019. 
#'         Significant input for the underlying \code{dwdradar::\link[dwdradar]{readRadarFile}} came
#'         from Henning Rust & Christoph Ritschel at FU Berlin.
#' @seealso \code{\link{readDWD}}, especially \code{\link{readDWD.binary}}\cr
#'   \url{https://wradlib.org} for much more extensive radar analysis in Python\cr
#'   Kompositformatbeschreibung at \url{https://www.dwd.de/DE/leistungen/radolan/radolan.html}
#'   for format description
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' # recent radar files 
#' rrf <- indexFTP("hourly/radolan/recent/bin", base=gridbase, dir=tempdir())
#' lrf <- dataDWD(rrf[773], base=gridbase, joinbf=TRUE, dir=tempdir(), read=FALSE)
#' r <- readDWD(lrf)
#' 
#' rp <- projectRasterDWD(r$dat)
#' raster::plot(rp, main=r$meta$date)
#' addBorders()
#' }
#' @param file      Name of file on harddrive, like e.g. 
#'                  DWDdata/hourly/radolan/recent/bin/
#'                  raa01-rw_10000-1802020250-dwd---bin.gz
#' @param gargs     Named list of arguments passed to 
#'                  \code{R.utils::\link[R.utils:compressFile]{gunzip}}. The internal 
#'                  defaults are: \code{remove=FALSE} (recommended to keep this
#'                  so \code{file} does not get deleted) and \code{skip=TRUE}
#'                  (which reads previously unzipped files as is).
#'                  If \code{file} has changed, you might want to use 
#'                  \code{gargs=list(skip=FALSE, overwrite=TRUE)}
#'                  or alternatively \code{gargs=list(temporary=TRUE)}.
#'                  The \code{gunzip} default \code{destname} means that the 
#'                  unzipped file is stored at the same path as \code{file}.
#'                  DEFAULT gargs: NULL
#' @param toraster  Logical: convert output (list of matrixes + meta informations)
#'                  to a list with data (\code{raster \link[raster]{stack}}) + 
#'                  meta (list from the first subfile, but with vector of dates)?
#'                  DEFAULT: TRUE
#' @param quiet     Suppress subfunction name message? 
#'                  DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots     Further arguments passed to \code{dwdradar::\link[dwdradar]{readRadarFile}}, 
#'                  i.e. \code{na} and \code{clutter}
readDWD.radar <- function(file, gargs=NULL, toraster=TRUE, quiet=rdwdquiet(), ...)
{
if(!quiet) message("Reading file with readDWD.radar().")
checkSuggestedPackage("dwdradar","rdwd:::readDWD.radar")
checkSuggestedPackage("R.utils", "rdwd:::readDWD.radar")
# gunzip arguments:
gdef <- list(filename=file, remove=FALSE, skip=TRUE)
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
rdata <- do.call(R.utils::gunzip, gfinal)
rf <- dwdradar::readRadarFile(rdata, ...)
if(toraster) checkSuggestedPackage("raster", "rdwd:::readDWD.radar with toraster=TRUE")
if(toraster) rf$dat <- raster::raster(rf$dat)
return(invisible(rf))
}



# ~ asc ----

#' @title read dwd gridded radolan asc data
#' @description read grid-interpolated radolan asc data. 
#' Intended to be called via \code{\link{readDWD}}.\cr
#' All layers (following \code{selection} if given) in all .tar.gz files are 
#' combined into a raster stack with \code{raster::\link[raster]{stack}}.\cr
#' To project the data, use \code{\link{projectRasterDWD}}
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, April 2019
#' @seealso \code{\link{readDWD}}
# @importFrom raster raster stack crs projection extent plot
#' @examples 
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' # File selection and download:
#' datadir <- localtestdir()
#' radbase <- paste0(gridbase,"/hourly/radolan/historical/asc/")
#' radfile <- "2018/RW-201809.tar" # 25 MB to download
#' file <- dataDWD(radfile, base=radbase, joinbf=TRUE, dir=datadir,
#'                 dbin=TRUE, read=FALSE) # download with mode=wb!!!
#'                 
#' #asc <- readDWD(file) # 4 GB in mem. ~ 20 secs unzip, 30 secs read, 10 min divide
#' asc <- readDWD(file, selection=1:5, dividebyten=TRUE)
#' asc <- projectRasterDWD(asc)
#' 
#' raster::plot(asc[[1]], main=names(asc)[1])
#' addBorders()
#' 
#' rng <- range(raster::cellStats(asc, "range"))
#' nframes <- 3 # raster::nlayers(asc) for all (time intensive!)
#' viddir <- paste0(tempdir(),"/RadolanVideo")
#' dir.create(viddir)
#' png(paste0(viddir,"/Radolan_%03d.png"), width=7, height=5, units="in", res=300)
#' dummy <- pbsapply(1:nframes, function(i) 
#'          raster::plot(asc[[i]], main=names(asc)[i], zlim=rng)) # 3 secs per layer
#' dev.off()
#' berryFunctions::openFile(paste0(viddir,"/Radolan_001.png"))
#' 
#' # Time series of a given point in space:
#' plot(as.vector(asc[800,800,]), type="l", xlab="Time [hours]")
#' 
#' # if dividebyten=FALSE, raster stores things out of memory in the exdir.
#' # by default, this is in tempdir, hence you would need to save asc manually:
#' # raster::writeRaster(asc, paste0(datadir,"/RW2018-09"), overwrite=TRUE) 
#' }
#' @param file        Name of file on harddrive, like e.g. 
#'                    DWDdata/grids_germany/hourly/radolan/historical/asc/
#'                    2018_RW-201809.tar.
#'                    Must have been downloaded with \code{mode="wb"}!
#' @param exdir       Directory to unzip into. Unpacked files existing therein
#'                    will not be untarred again, saving up to 15 secs per file.
#'                    DEFAULT: NULL (subfolder of \code{\link{tempdir}()})
#' @param dividebyten Divide numerical values by 10? 
#'                    If dividebyten=FALSE and exdir left at NULL (tempdir), save 
#'                    the result on disc with \code{raster::\link[raster]{writeRaster}}.
#'                    Accessing out-of-memory raster objects won't work if 
#'                    exdir is removed! -> Error in .local(.Object, ...)
#'                    DEFAULT: TRUE
#' @param quiet       Suppress progress messages? 
#'                    DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param progbar     Show progress bars? \code{\link{readDWD}} will
#'                    keep progbar=TRUE for asc files, even if length(file)==1.
#'                    DEFAULT: !quiet, i.e. TRUE
#' @param selection   Optionally read only a subset of the ~24*31=744 files.
#'                    Called as \code{f[selection]}. DEFAULT: NULL (ignored)
#' @param \dots       Further arguments passed to \code{raster::\link[raster]{raster}}
readDWD.asc <- function(file, exdir=NULL, dividebyten=TRUE, 
                        selection=NULL, quiet=rdwdquiet(), progbar=!quiet, ...)
{
if(!quiet) message("Reading file with readDWD.asc().")
checkSuggestedPackage("raster", "rdwd:::readDWD.asc")
if(progbar) lapply <- pbapply::pblapply
# prepare to untar data (two layers):
fn <- tools::file_path_sans_ext(basename(file))
if(is.null(exdir)) exdir <- paste0(tempdir(),"/", fn)
#
# untar layer 1:
daydir <- paste0(exdir,"/dayfiles")
untar(file, exdir=daydir) # 30/31 .tar.gz files (one for each day). overwrites existing files
dayfiles <- dir(daydir, full.names=TRUE)
#
# untar layer 2:
if(!quiet) message("\nChecking if already unpacked: ", file, "...")
to_untar <- lapply(dayfiles, untar, list=TRUE)
untarred <- dir(exdir, pattern=".asc$")
to_untar <- !sapply(to_untar, function(x) all(x %in% untarred))
if(any(to_untar)){
  if(!quiet) message("Unpacking tar files into ",exdir,"...")
  lapply(dayfiles[to_untar], untar, exdir=exdir) 
} else if(!quiet) message("Tar file was already unpacked into ",exdir," :)")
# yields 31 * 24 .asc files each 1.7MB, takes ~20 secs
#
#
# read data (hourly files):
f <- dir(exdir, pattern=".asc$", full.names=TRUE) # 720 files
if(!is.null(selection)) f <- f[selection]
if(!quiet) message("Reading ",length(f)," files...")
dat <- lapply(f, raster::raster, ...)
#
# divide by ten (takes ~9 min!)
if(!quiet & dividebyten) message("Dividing values by ten...")
if(dividebyten) dat <- lapply(dat, function(x) x/10)
#
# stack layers:
dat <- raster::stack(dat)
#
# output:
return(invisible(dat))
}
