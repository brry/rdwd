# read dwd ----

#' @title Process data from the DWD CDC FTP Server
#' @description Read climate data that was downloaded with [dataDWD()].
#' The data is unzipped and subsequently, the file(s) are read, processed and
#' returned as a data.frame / raster object.\cr\cr
#' For observational data, new users are advised to set `varnames=TRUE` 
#' to obtain more informative column names.\cr\cr
#' `readDWD` will call internal (but documented) subfunctions depending on the
#' argument `type`, see the overview in [fileType()].\cr\cr
#' Not all arguments to `readDWD` are used for all subfunctions, e.g.
#' `fread` is used only by [`readDWD.data`], while `dividebyten`
#' is used in [`readDWD.raster`] and [`readDWD.asc`].\cr\cr
#' `file` can be a vector with several filenames. Most other arguments can
#' also be a vector and will be recycled to the length of `file`.
#' 
#' @return For observational data, an invisible data.frame of the desired dataset,
#'         or a named list of data.frames if length(file) > 1.\cr
#'         For gridded data, raster objects.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016, Winter 2018/19
#' @seealso [dataDWD()], [readVars()], [readMeta()], [selectDWD()], [fileType()]\cr
#'          <https://bookdown.org/brry/rdwd>
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf untar write.table
#' @importFrom berryFunctions checkFile na9 traceCall l2df owa
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD and readDWD.* subfunctions
#' 
#' @param file   Char (vector): name(s) of the file(s) downloaded with
#'               [dataDWD()],
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip" or
#'               "~/DWDdata/RR_Stundenwerte_Beschreibung_Stationen.txt"
#' @param type   Character (vector) determining which subfunction to call.
#'               DEFAULT:  [`fileType`]`(file)`.
#' @param varnames Logical (vector): Expand column names?
#'               Only used in [readDWD.data()]. 
#'               DEFAULT: FALSE (for backward compatibility)
#' @param fread  Logical (vector): read fast? Used in [readDWD.data()].
#'               DEFAULT: NA
#' @param format,tz Format and time zone of time stamps, see [readDWD.data()]
#' @param dividebyten Logical (vector): Divide the values in raster files by ten?
#'               Used in [readDWD.raster()] and [readDWD.asc()].
#'               DEFAULT: TRUE
#' @param var    var for [readDWD.nc()]. DEFAULT: ""
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: !quiet
#' @param quiet  Logical: suppress messages? DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots  Further arguments passed to the internal `readDWD.*`
#'               subfunctions (see [`fileType`]) and from those to the 
#'               underlying actual reading functions
#' 
readDWD <- function(
file,
type=fileType(file),
varnames=FALSE,
fread=NA,
format=NA,
tz="GMT",
dividebyten=TRUE,
var="",
progbar=!quiet,
quiet=rdwdquiet(),
...
)
{
# recycle arguments:
len <- length(file)
if(missing(progbar) & len==1 & all(type!="binary") & all(type!="asc")) progbar <- FALSE
# fast reading with fread:
if(anyNA(fread))
  {
  haspack <- requireNamespace("data.table", quietly=TRUE)
  if(haspack && Sys.which("unzip")=="") 
    warning("R package 'data.table' available for fast reading of files, ",
            "but system command 'unzip' could not be found. Now reading slowly.\n",
            "See   https://bookdown.org/brry/rdwd/fread.html")
  fread[is.na(fread)] <- haspack && Sys.which("unzip")!=""
  }
if(any(fread))
  {
  checkSuggestedPackage("data.table", "rdwd::readDWD with fread=TRUE")
  checkSuggestedPackage("bit64",      "rdwd::readDWD with fread=TRUE")
  if(Sys.which("unzip")=="") warning("system command 'unzip' could not be found. ",
                                     "Expect trouble with data.table::fread.\n",
                                     "See   https://bookdown.org/brry/rdwd/fread.html")
  }
if(len>1)
  {
  fread       <- rep(fread,       length.out=len)
  varnames    <- rep(varnames,    length.out=len)
  var         <- rep(var,         length.out=len)
  format      <- rep(format,      length.out=len)
  tz          <- rep(tz,          length.out=len)
  dividebyten <- rep(dividebyten, length.out=len)
  type        <- rep(type,        length.out=len)
  }
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
#
checkFile(file)
# Handle German Umlaute:
if(any(type=="meta")) # faster to change locale once here, instead of in each readDWD.meta call
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
# subfunctions message:
if(!quiet)
{
nt <- function(x, pre="readDWD.", post="()") # nt: nice table
  {
  if(length(unique(x))==1) return(paste0(pre,x[1],post))
  x <- table(x)
  x <- paste0(names(x), " (", x, ")")
  paste0(pre,x, collapse=" / ")
  }
msg <- paste0("Reading ",length(file)," file", if(length(file)>1)"s", " with ",nt(type))
if(any(type=="data")) msg <- paste0(msg, " and fread=",nt(fread,"",""))
message(msg, " ...")
}
#
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
# call subfunction:
if(type[i]=="multia") return(readDWD.multia(file[i], quiet=quiet, ...))
if(type[i]=="stand")  return(readDWD.stand( file[i], quiet=quiet, ...))
if(type[i]=="meta")   return(readDWD.meta(  file[i], quiet=quiet, ...))
if(type[i]=="binary") return(readDWD.binary(file[i], quiet=quiet, progbar=progbar, ...))
if(type[i]=="raster") return(readDWD.raster(file[i], quiet=quiet, dividebyten=dividebyten[i], ...))
if(type[i]=="nc")     return(readDWD.nc(    file[i], quiet=quiet, var=var[i], ...))
if(type[i]=="radar")  return(readDWD.radar( file[i], quiet=quiet, ...))
if(type[i]=="asc")    return(readDWD.asc(   file[i], quiet=quiet, progbar=progbar, dividebyten=dividebyten[i], ...))
if(type[i]=="grib2")  return(readDWD.grib2( file[i], quiet=quiet, ...))
if(type[i]=="data")   return(readDWD.data(  file[i], quiet=quiet, fread=fread[i], varnames=varnames[i], format=format[i], tz=tz[i], ...))
stop("invalid type (",type[i],") given for file '",file[i],"'. See  ?fileType")
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
#' Intended to be called via [readDWD()].
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso [readDWD()], Examples in [dataDWD()]
#' @param file     Name of file on harddrive, like e.g.
#'                 DWDdata/daily_kl_recent_tageswerte_KL_03987_akt.zip
#' @param fread    Logical: read faster with [data.table::fread]?
#'                 When reading many large historical files, speedup is significant.
#'                 When called from [readDWD()], `fread=NA` can also be used, which means 
#'                 TRUE if R package `data.table` and system command `unzip` are available.
#'                 Hint for Windows users: `unzip` comes with Rtools.
#'                 DEFAULT: FALSE
#' @param varnames Logical (vector): add a short description to the DWD variable
#'                 abbreviations in the column names?
#'                 E.g. change `FX,TNK` to `FX.Windspitze,TNK.Lufttemperatur_Min`,
#'                 see [newColumnNames()].
#'                 DEFAULT: FALSE (for backwards compatibility)
#' @param format   Char (vector): Format passed to [as.POSIXct()] (see [strptime()])
#'                 to convert the date/time column to POSIX time format.\cr
#'                 If NULL, no conversion is performed (date stays a factor).
#'                 If NA, `readDWD` tries to find a suitable format based
#'                 on the number of characters. DEFAULT: NA
#' @param tz       Char (vector): time zone for [as.POSIXct()].
#'                 "" is the current time zone, and "GMT" is UTC (Universal Time,
#'                 Coordinated). DEFAULT: "GMT"
#' @param quiet    Suppress empty file warnings?
#'                 DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots    Further arguments passed to [read.table()] or [data.table::fread()]
readDWD.data <- function(file, fread=FALSE, varnames=FALSE, format=NA, tz="GMT",
                         quiet=rdwdquiet(), ...)
{
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
#' Intended to be called via [readDWD()].\cr
#' All other observational data at [`dwdbase`] can be read
#' with [readDWD.data()], except for the multi_annual and
#' subdaily/standard_format data.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2019
#' @seealso [readDWD()]
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
#' berryFunctions::linReg(ma$Stationshoehe, ma$Jahr, main="annual average ~ elevation")
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
#' load(system.file("extdata/DEU.rda", package="rdwd"))
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
#' @param fileEncoding [read.table()] file encoding.
#'              DEFAULT: "latin1" (needed on Linux, optional but not hurting on windows)
#' @param comment.char [read.table()] comment character.
#'              DEFAULT: "\\032" (needed 2019-04 to ignore the binary
#'              control character at the end of multi_annual files)
#' @param quiet Ignored.
#'              DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots Further arguments passed to [read.table()]
readDWD.multia <- function(file, fileEncoding="latin1", comment.char="\032",
                           quiet=rdwdquiet(), ...)
{
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
#' Intended to be called via [readDWD()].\cr
#' All other observational data at [`dwdbase`] can be read
#' with [readDWD.data()], except for the multi_annual and
#' subdaily/standard_format data.
#' @return data.frame with column names as per [`formatIndex`].
#' "Q"-columns have "_parameter" appended to their name. A "Date" column has been added.
#' NA-indicators have been processed into NAs.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2019
#' @seealso [readDWD()]
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
#' @param fast  Logical: use [readr::read_fwf()] instead of [read.fwf()]?
#'              Takes 0.1 instead of 20 seconds but requires package to be installed.
#'              if fast=TRUE, `fileEncoding` is ignored.
#'              DEFAULT: TRUE
#' @param fileEncoding [read.table()] file encoding.
#'              DEFAULT: "latin1" (potentially needed on Linux,
#'              optional but not hurting on windows)
#' @param formIndex Single object: Index used to select column widts and NA values.
#'              To use a current / custom index, see the source code of
#'              [updateIndexes()] at
#'              <https://github.com/brry/rdwd/blob/master/R/updateIndexes.R>.
#'              DEFAULT: [`formatIndex`]
#' @param quiet Ignored.
#'              DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots Further arguments passed to [read.fwf()] or [readr::read_fwf()]
readDWD.stand <- function(file, fast=TRUE, fileEncoding="latin1",
                          formIndex=formatIndex, quiet=rdwdquiet(), ...)
{
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
#'  Intended to be called via [readDWD()].\cr
#'  Column widths for [read.fwf()] are computed internally.\cr
#'  if(any(meta)), [readDWD()] tries to set the locale to German
#'  (to handle Umlaute correctly). It is hence not recommended to call
#'  `rdwd:::readDWD.meta` directly on a file!\cr
#'  Names can later be changed to ascii with
#'  [berryFunctions::convertUmlaut()].
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso [readDWD()]
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
#' @param quiet Ignored.
#'              DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots Further arguments passed to [read.fwf()]
readDWD.meta <- function(file, quiet=rdwdquiet(), ...)
{
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
classes <- c("integer", "integer", "integer", "integer", "numeric", "numeric", "character", "character")
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
#' Intended to be called via [readDWD()].\cr
#' @return list depending on argument `toraster`, see there for details
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018.
#'         Significant input for the underlying [dwdradar::readRadarFile()] came
#'         from Henning Rust & Christoph Ritschel at FU Berlin.
#' @seealso [readDWD()], especially [readDWD.radar()]\cr
#'   <https://wradlib.org> for much more extensive radar analysis in Python\cr
#'   Kompositformatbeschreibung at <https://www.dwd.de/DE/leistungen/radolan/radolan.html>
#'   for format description
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' # SF file as example: ----
#' 
#' SF_link <- "/daily/radolan/historical/bin/2017/SF201712.tar.gz"
#' SF_file <- dataDWD(url=SF_link, base=gridbase, joinbf=TRUE,   # 204 MB
#'                      dir=localtestdir(), read=FALSE)
#' # exdir radardir set to speed up my tests:
#' SF_exdir <- "C:/Users/berry/Desktop/DWDbinarySF"
#' if(!file.exists(SF_exdir)) SF_exdir <- tempdir()
#' # no need to read all 24*31=744 files, so setting selection:
#' SF_rad <- readDWD(SF_file, selection=1:10, exdir=SF_exdir) #with toraster=TRUE
#' if(length(SF_rad)!=2) stop("length(SF_rad) should be 2, but is ", length(SF_rad))
#' 
#' SF_radp <- plotRadar(SF_rad$dat, layer=1:3, main=SF_rad$meta$date)
#' plotRadar(SF_radp, layer=1, project=FALSE)
#' 
#' # RW file as example: ----
#' 
#' RW_link <- "hourly/radolan/reproc/2017_002/bin/2017/RW2017.002_201712.tar.gz"
#' RW_file <- dataDWD(url=RW_link, base=gridbase, joinbf=TRUE,   # 25 MB
#'                   dir=localtestdir(), read=FALSE)
#' RW_exdir <- "C:/Users/berry/Desktop/DWDbinaryRW"
#' if(!file.exists(RW_exdir)) RW_exdir <- tempdir()
#' RW_rad <- readDWD(RW_file, selection=1:10, exdir=RW_exdir)
#' RW_radp <- plotRadar(RW_rad$dat[[1]], main=RW_rad$meta$date[1], extent="rw")
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
#'                  will be unpacked with [untar()]. Note that exdir
#'                  size will be around 1.1 GB. exdir can contain other files,
#'                  these will be ignored for the actual reading with
#'                  [dwdradar::readRadarFile()].
#'                  DEFAULT exdir: sub(".tar.gz$", "", file)
#' @param toraster  Logical: convert output (list of matrixes + meta informations)
#'                  to a list with dat ([`raster::stack`]) +
#'                  meta (list from the first subfile, but with vector of dates)?
#'                  DEFAULT: TRUE
#' @param quiet     Suppress progress messages?
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param progbar   Show progress bars? [readDWD()] will
#'                  keep progbar=TRUE for binary files, even if length(file)==1.
#'                  DEFAULT: !quiet, i.e. TRUE
#' @param selection Optionally read only a subset of the ~24*31=744 files.
#'                  Called as `f[selection]`. DEFAULT: NULL (ignored)
#' @param \dots     Further arguments passed to [dwdradar::readRadarFile()],
#'                  i.e. `na` and `clutter`
readDWD.binary <- function(file, exdir=sub(".tar.gz$", "", file), toraster=TRUE,
                           quiet=rdwdquiet(), progbar=!quiet, selection=NULL, ...)
{
checkSuggestedPackage("dwdradar", "rdwd:::readDWD.binary")
pmessage <- function(...) if(!quiet) message(...)
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
#' Intended to be called via [readDWD()].\cr
#' Note that `R.utils` must be installed to unzip the .asc.gz files.
#' @return [`raster::raster`] object
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018
#' @seealso [readDWD()]
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
#' plotRadar(rf,proj="seasonal", extent=rf@extent)
#' 
#' testthat::expect_equal(raster::cellStats(rf, range), c(-8.2,4.4))
#' rf10 <- readDWD(localfiles[1], dividebyten=FALSE)
#' raster::plot(rf10)
#' testthat::expect_equal(raster::cellStats(rf10, range), c(-82,44))
#' }
#' @param file        Name of file on harddrive, like e.g.
#'                    DWDdata/grids_germany/seasonal/air_temperature_mean/
#'                    16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' @param gargs       Named list of arguments passed to [R.utils::gunzip()].
#'                    The internal defaults are: `remove=FALSE` (recommended to
#'                    keep this so `file` does not get deleted) and `skip=TRUE`
#'                    (which reads previously unzipped files as is).
#'                    If `file` has changed, you might want to use
#'                    `gargs=list(skip=FALSE, overwrite=TRUE)`
#'                    or alternatively `gargs=list(temporary=TRUE)`.
#'                    The `gunzip` default `destname` means that the
#'                    unzipped file is stored at the same path as `file`.
#'                    DEFAULT gargs: NULL
#' @param dividebyten Logical: Divide the numerical values by 10?
#'                    DEFAULT: TRUE
#' @param quiet       Ignored.
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots       Further arguments passed to [raster::raster()]
readDWD.raster <- function(file, gargs=NULL, dividebyten, quiet=rdwdquiet(), ...)
{
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
#' Intended to be called via [readDWD()].\cr
#' Note that `R.utils` and `ncdf4` must be installed to unzip and read the .nc.gz files.
#' @return [raster::brick()] object. Alternatively,
#' if toraster=FALSE, a list with time, lat, lon, var, varname, file and cdf.
#' **cdf** is the output of [ncdf4::nc_open()].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso [readDWD()]
#' @importFrom berryFunctions removeSpace
#' @importFrom utils menu tail capture.output
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' library(berryFunctions) # for seqPal and colPointsLegend
#' 
#' url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
#' url <- "daily/Project_TRY/humidity/RH_199509_daymean.nc.gz"    # 25 MB
#' file <- dataDWD(url, base=gridbase, joinbf=TRUE, dir=localtestdir(), read=FALSE)
#' nc <- readDWD(file)
#' ncp <- plotRadar(nc, main=paste(nc@title, nc@z[[1]]), layer=1:3,
#'                  col=seqPal(), proj="nc", extent="nc")
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
#'                    [R.utils::gunzip()], see [readDWD.raster()]. DEFAULT: NULL
#' @param toraster    Read file with [raster::brick()]?
#'                    All further arguments will be ignored. Specify e.g. `var` 
#'                    through \dots as `varname`. DEFAULT: TRUE
#' @param var         if toraster=FALSE: Charstring with name of variable to be read with
#'                    [ncdf4::ncvar_get()]. If not available,
#'                    an interactive selection is presented.
#'                    DEFAULT: "" (last variable)
#' @param quiet       Logical: Suppress time conversion failure warning?
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots       Further arguments passed to [raster::brick()] or [ncdf4::nc_open()]
#' 
readDWD.nc <- function(file, gargs=NULL, var="", toraster=TRUE, quiet=rdwdquiet(), ...)
{
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
  # ignore ncdf print, see https://github.com/rspatial/raster/issues/143
  capture.output(out <- raster::brick(ncfile, ...))
  return(invisible(out))
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
 start <- strsplit(berryFunctions::removeSpace(start), " ")[[1]][1]
 start <- as.Date(start)
 time <- start + ncdf4::ncvar_get(mycdf,'time')
 } else
 {
 start <- berryFunctions::removeSpace(sub("hours since", "", unit)) # always hours?
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
#' Intended to be called via [readDWD()].\cr
#' @return Invisible list with `dat` (matrix or raster, depending on `toraster`)
#' and `meta` (list with elements from header)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019.
#'         Significant input for the underlying [dwdradar::readRadarFile()] came
#'         from Henning Rust & Christoph Ritschel at FU Berlin.
#' @seealso [readDWD()], especially [readDWD.binary()]\cr
#'   <https://wradlib.org> for much more extensive radar analysis in Python\cr
#'   Kompositformatbeschreibung at <https://www.dwd.de/DE/leistungen/radolan/radolan.html>
#'   for format description
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' # recent radar files
#' rrf <- indexFTP("hourly/radolan/recent/bin", base=gridbase, dir=tempdir())
#' lrf <- dataDWD(rrf[773], base=gridbase, joinbf=TRUE, dir=tempdir(), read=FALSE)
#' r <- readDWD(lrf)
#' 
#' plotRadar(r$dat, main=r$meta$date)
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  DWDdata/hourly/radolan/recent/bin/
#'                  raa01-rw_10000-1802020250-dwd---bin.gz
#' @param gargs     Named list of arguments passed to
#'                  [R.utils::gunzip()], see [readDWD.raster()]. DEFAULT: NULL
#' @param toraster  Logical: convert output (list of matrixes + meta informations)
#'                  to a list with data ([`raster::stack`]) +
#'                  meta (list from the first subfile, but with vector of dates)?
#'                  DEFAULT: TRUE
#' @param quiet     Ignored.
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots     Further arguments passed to [dwdradar::readRadarFile()],
#'                  i.e. `na` and `clutter`
readDWD.radar <- function(file, gargs=NULL, toraster=TRUE, quiet=rdwdquiet(), ...)
{
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
#' Intended to be called via [readDWD()].\cr
#' All layers (following `selection` if given) in all .tar.gz files are
#' combined into a raster stack with [raster::stack()].\cr
#' To project the data, use [projectRasterDWD()]
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, April 2019
#' @seealso [readDWD()]
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

#' plotRadar(asc[[1]], main=names(asc)[1])
#' 
#' viddir <- paste0(tempdir(),"/RadolanVideo")
#' dir.create(viddir)
#' png(paste0(viddir,"/Radolan_%03d.png"), width=7, height=5, units="in", res=300)
#' plotRadar(asc, layer=1:3, main=names(asc)) # 3 secs per layer
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
#'                    Must have been downloaded with `mode="wb"`!
#' @param exdir       Directory to unzip into. Unpacked files existing therein
#'                    will not be untarred again, saving up to 15 secs per file.
#'                    DEFAULT: NULL (subfolder of [tempdir()])
#' @param dividebyten Divide numerical values by 10?
#'                    If dividebyten=FALSE and exdir left at NULL (tempdir), save
#'                    the result on disc with [raster::writeRaster()].
#'                    Accessing out-of-memory raster objects won't work if
#'                    exdir is removed! -> Error in .local(.Object, ...)
#'                    DEFAULT: TRUE
#' @param quiet       Suppress progress messages?
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param progbar     Show progress bars? [readDWD()] will
#'                    keep progbar=TRUE for asc files, even if length(file)==1.
#'                    DEFAULT: !quiet, i.e. TRUE
#' @param selection   Optionally read only a subset of the ~24*31=744 files.
#'                    Called as `f[selection]`. DEFAULT: NULL (ignored)
#' @param \dots       Further arguments passed to [raster::raster()]
readDWD.asc <- function(file, exdir=NULL, dividebyten=TRUE,
                        selection=NULL, quiet=rdwdquiet(), progbar=!quiet, ...)
{
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



# ~ grib2 ----

#' @title read nwp forecast data
#' @description read gridded numerical weather prediction data.
#' Intended to be called via [readDWD()].\cr
#' @return rgdal or raster object, depending on `toraster`
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2021.
#' @seealso [readDWD()]\cr
#' <https://www.dwd.de/EN/ourservices/nwp_forecast_data/nwp_forecast_data.html>\cr
#' <https://www.dwd.de/EN/aboutus/it/functions/Teasergroup/grib.html>\cr
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' nwp_t2m_base <- "ftp://ftp-cdc.dwd.de/weather/nwp/cosmo-d2/grib/00/t_2m/"
#' nwp_urls <- indexFTP("", base=nwp_t2m_base, dir=tempdir())
#' nwp_file <- dataDWD(nwp_urls[6], base=nwp_t2m_base, dir=tempdir(),
#'                     joinbf=TRUE, dbin=TRUE, read=FALSE)
#' nwp_data <- readDWD(nwp_file, quiet=TRUE)
#' plotRadar(nwp_data, project=FALSE)
#' 
#' nwp_data_rgdal <- readDWD(nwp_file, toraster=FALSE)
#' sp::plot(nwp_data_rgdal)
#' 
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  cosmo-d2_germany_regular-lat-lon_single-level_2021010100_005_T_2M.grib2.bz2
#' @param bargs     Named list of arguments passed to
#'                  [R.utils::bunzip2()], see `gargs` in [readDWD.raster()]. DEFAULT: NULL
#' @param toraster  Logical: convert [rgdal::readGDAL] output with [raster::raster()]?
#'                  DEFAULT: TRUE
#' @param quiet     Silence readGDAL completely, including warnings on 
#'                  discarded ellps / datum. 
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots     Further arguments passed to [rgdal::readGDAL()],
readDWD.grib2 <- function(file, bargs=NULL, toraster=TRUE, quiet=rdwdquiet(), ...)
{
checkSuggestedPackage("R.utils", "rdwd:::readDWD.grib2")
checkSuggestedPackage("rgdal"  , "rdwd:::readDWD.grib2")
# bunzip arguments:
bdef <- list(filename=file, remove=FALSE, skip=TRUE)
bfinal <- berryFunctions::owa(bdef, bargs, "filename")
# unzip:
bdata <- do.call(R.utils::bunzip2, bfinal)
# rgdal reading:
out <- if(!quiet)    rgdal::readGDAL(bdata,              ...) else
    suppressWarnings(rgdal::readGDAL(bdata, silent=TRUE, ...))
# conversion to raster:
if(toraster) 
  {
  checkSuggestedPackage("raster", "rdwd:::readDWD.grib2 with toraster=TRUE")
  out <- raster::raster(out)
  }
# Output:
return(invisible(out))
}
