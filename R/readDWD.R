# read dwd ----

#' @title Process data from the DWD CDC FTP Server
#' @description Read climate data that was downloaded with [dataDWD()].
#' The data is unzipped and subsequently, the file(s) are read, processed and
#' returned as a data.frame / terra raster object.\cr\cr
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
#'         For gridded data, terra raster objects.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016, Winter 2018/19
#' @seealso [dataDWD()], [readVars()], [readMeta()], [selectDWD()], [fileType()]\cr
#'          <https://bookdown.org/brry/rdwd>
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf untar write.table
#' @importFrom berryFunctions checkFile na9 twarning tstop l2df owa
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
#' @param hr     Integer code to merge historical and recent file. 
#'               Used here, but documented in detail in [readDWD.data()].
#'               DEFAULT: 0 (ignore argument)
#' @param dividebyten Logical (vector): Divide the values in raster files by ten?
#'               That way, \[1/10 mm] gets transformed to \[mm] units.
#'               Used in [readDWD.radar()], [readDWD.raster()] and [readDWD.asc()].
#'               DEFAULT: TRUE
#' @param var    var for [readDWD.nc()]. DEFAULT: ""
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE,
#'               unless binary files are to be read.
#'               DEFAULT: !quiet
#' @param quiet  Logical: suppress messages? DEFAULT: FALSE through [rdwdquiet()]
#' @param quietread Logical: suppress message like 
#'               "Reading 1 file with readDWD.data() and fread=TRUE ...".
#'               DEFAULT: `quiet`
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
hr=0,
dividebyten=TRUE,
var="",
progbar=!quiet,
quiet=rdwdquiet(),
quietread=quiet,
...
)
{
# recycle arguments:
len <- length(file)
if(missing(progbar) & len==1 & all(!type %in% c("binary","asc","rklim"))) progbar <- FALSE

wrongtype <- !type %in% validFileTypes # should be caught by fileType, but just in case
if(any(wrongtype)) tstop("invalid type (",type[wrongtype][1],") given for file '",
                         file[wrongtype][1],"'. See  ?fileType")

if("data" %in% type)
{
if(!is.numeric(hr)) tstop("hr must be an integer, not ", toString(class(hr)), 
                          " with the value ", toString(hr))
# fast reading with fread:
if(anyNA(fread))
  {
  haspack <- requireNamespace("data.table", quietly=TRUE)
  if(haspack && Sys.which("unzip")=="") 
   twarning("R package 'data.table' available for fast reading of files, ",
            "but system command 'unzip' could not be found. Now reading slowly.\n",
            "See   https://bookdown.org/brry/rdwd/fread.html")
  fread[is.na(fread)] <- haspack && Sys.which("unzip")!=""
  }
if(any(fread))
  {
  checkSuggestedPackage("data.table", "rdwd::readDWD with fread=TRUE")
  checkSuggestedPackage("bit64",      "rdwd::readDWD with fread=TRUE")
  if(Sys.which("unzip")=="") twarning("system command 'unzip' could not be found. ",
                                     "Expect trouble with data.table::fread.\n",
                                     "See   https://bookdown.org/brry/rdwd/fread.html")
  }
} 
# end hr + fread / unzip checks

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

# subfunctions message:
if(!quietread)
{
nt <- function(x, pre="readDWD.", post="()") # nt: nice table
  {
  if(length(unique(x))==1) return(paste0(pre,x[1],post))
  x <- table(x)
  x <- paste0(names(x), " (", x, ")")
  paste0(pre,x, collapse=" / ")
  }
msg <- paste0("Reading ",length(file)," file", if(length(file)>1)"s", " with ",nt(type))
if(any("data" %in% type)) msg <- paste0(msg, " and fread=",nt(fread,"",""))
if(any(c("radar", "raster", "asc") %in% type)) 
  msg <- paste0(msg, " and dividebyten=",nt(dividebyten,"",""))
if(any("grib2" %in% type))
 message(msg, appendLF=FALSE) else
message(msg, " ...")
}

# loop over each filename
readDWDloopfun <- function(i, ...){
arg <- NULL
if(type[i]=="data")   arg <- list(fread=fread[i], varnames=varnames[i], format=format[i], tz=tz[i])
if(type[i]=="binary") arg <- list(progbar=progbar)
if(type[i]=="rklim")  arg <- list(progbar=progbar)
if(type[i]=="raster") arg <- list(dividebyten=dividebyten[i])
if(type[i]=="radar")  arg <- list(dividebyten=dividebyten[i])
if(type[i]=="nc")     arg <- list(var=var[i])
if(type[i]=="asc")    arg <- list(progbar=progbar, dividebyten=dividebyten[i])
# arguments to subfunction:
arg <- c(list(file=file[i], quiet=quiet, ...), arg)
# call subfunction:
out <- try(do.call(paste0("readDWD.",type[i]), arg), silent=TRUE)
if(inherits(out, "try-error")) tstop("failure reading file:\n", file[i], "\n", out)
return(out)
}
output <- lapply(seq_along(file), readDWDloopfun, ...)

names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
output <- invisible(output)

# hr for merging historical + recent data:
if("data" %in% type && hr>0){
if(!quiet) message("merging historical and recent file.")
h <- grep("historical", names(output), fixed=TRUE)
if(length(h)<1) tstop("hr=", hr, ", but ", length(h), " files have 'historical' in the name (must be 1).")
if(length(h)!=1) 
{
fn <- basename(file[h])
fnp <- strsplit(fn,"_")[[1]] # fnp: file name path
# fnp <- paste0(fnp[1],"/",fnp[2],"/",substr(fnp[3],1,1))
fnp <- paste(fnp[1:2], collapse="/")
fnid <- as.numeric(sub("\\D*(\\d{5}).*", "\\1", fn)) # first five digits after non-digits
fnid <- unique(fnid)
if(length(fnid)>1) tstop("hr=", hr, ", but ", length(fnid), " ids are given:", 
                         truncMessage(fnid, prefix=""))
fny <- sub(".*(\\d{8}).*(\\d{8})\\D*$", "\\1-\\2", fn) # fn years
fnu <- base::lapply(strsplit(fny,"-"), as.Date, format="%Y%m%d") # fn to be used
fnu <- which.max(sapply(fnu, diff))
twarning("For ID '",fnid,"' at path '",fnp,"', ",length(h)," files have 'historical' in the name.\n",
         "Using longest range ",fny[fnu]," (not ",toString(fny[-fnu]),"). ",
         "Use hr=0 to read all data.")
h <- h[fnu] # for selection in output[c(h,r)]
}
r <- grep("recent", names(output), fixed=TRUE)
if(length(r)!=1) tstop("hr=", hr, ", but ", length(r), " files have 'recent' in the name (must be 1).")
output <- output[c(h,r)] # to have the right order
output <- do.call(rbind, output) # historical and recent data
if(hr>=2) output <- output[!duplicated(output$MESS_DATUM),] # keep only hist for overlap
if(hr>=2) rownames(output) <- NULL
if(hr>=3) output[,c("QN_3", "QN_4", "eor")] <- NULL # unneeded columns
if(hr>=4) output$STATIONS_ID <- NULL
} # End of hr merging

return(output)
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
#'                 See <https://bookdown.org/brry/rdwd/fread.html>
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
#' @param hr       Integer code to automatically merge historical and recent datasets.
#'                 If set, `readDWD` returns a data.frame instead of a list.
#'                 If multiple historical files are present, 
#'                 the longest date range (per file name) is used.
#'                 This is not actually used in `readDWD.data`, but in [readDWD()].\cr
#'                 0 (default): ignore this argument\cr
#'                 1: sort by hr (if given) + merge\cr
#'                 2: also remove duplicated dates from recent\cr
#'                 3: also remove columns QN3,QN4,eor\cr
#'                 4: also remove column STATIONS_ID\cr
#'                 DEFAULT: 0
#' @param quiet    Suppress empty file warnings?
#'                 DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots    Further arguments passed to [read.table()] or [data.table::fread()]
readDWD.data <- function(file, fread=FALSE, varnames=FALSE, format=NA, tz="GMT",
                         hr=0, quiet=rdwdquiet(), ...)
{
if(grepl("meta_data_Meta_Daten", file)) tstop("This 'meta_data_Meta_Daten' file should be read with readMeta(",file,")")
if(fread)
  {
  # http://dsnotes.com/post/2017-01-27-lessons-learned-from-outbrain-click-prediction-kaggle-competition/
  fp <- unzip(file, list=TRUE) # file produkt*, the actual datafile
  fp <- fp$Name[grepl("produkt",fp$Name)]
  if(length(fp)!=1) tstop("There should be a single 'produkt*' file, but there are ",
                        length(fp), " in\n  ", file, "\n  Consider re-downloading (with force=TRUE).")
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
if(length(f)!=1) tstop("There should be a single 'produkt*' file, but there are ",
                      length(f), " in\n  ", file, "\n  Consider re-downloading (with force=TRUE).")
dat <- read.table(f, na.strings=na9(), header=TRUE, sep=";", as.is=FALSE, ...)
} # end if(!fread)
#
if(varnames)  dat <- newColumnNames(dat)
# return if file is empty, e.g. for daily/more_precip/hist_05988 2019-05-16:
if(nrow(dat)==0)
  {
  if(!quiet) twarning("File contains no rows: ", file)
  return(dat)
  }
# process time-stamp: http://stackoverflow.com/a/13022441
if(!is.null(format))
  {
  # for res=monthly data:
  if("MESS_DATUM_BEGINN" %in% colnames(dat))
    dat <- cbind(dat[,1, drop=FALSE], MESS_DATUM=dat$MESS_DATUM_BEGINN + 14, dat[,-1])
  if(!"MESS_DATUM" %in% colnames(dat))
    twarning("There is no column 'MESS_DATUM' in ",file) else
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



# ~ deriv ----

#' @title read derived dwd data
#' @description Read dwd data from /CDC/derived_germany/.
#' Intended to be called via [readDWD()].
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso [readDWD()], https://bookdown.org/brry/rdwd/use-case-derived-data.html
#' @param file     Name of file on harddrive, like e.g.
#'                 DWDdata/soil_daily_historical_derived_germany_soil_daily_historical_3987.txt.gz
#' @param gargs    If fread=FALSE: Named list of arguments passed to
#'                 [R.utils::gunzip()], see [readDWD.raster()]. DEFAULT: NULL
#' @param todate   Logical: Convert char column 'Datum' or 'Monat' with [as.Date()]?
#'                 The format is currently hard-coded. Monthly data gets mapped to yyyy-mm-15
#'                 DEFAULT: TRUE
#' @param quiet    Ignored.
#'                 DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots    Further arguments passed to [read.table()] or [data.table::fread()]
readDWD.deriv <- function(file, gargs=NULL, todate=TRUE, quiet=rdwdquiet(), ...)
{
# I tried data.table::fread(file, na.strings=na9(nspace=0), header=TRUE, sep=";", data.table=FALSE, ...)
# but it's not really faster in most cases. 
# gunzip is very fast in subsequent calls, only slow in the first call.
checkSuggestedPackage("R.utils", "rdwd:::readDWD.derived")
gdef <- list(filename=file, remove=FALSE, skip=TRUE) # gunzip arguments
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
file2 <- do.call(R.utils::gunzip, gfinal)
dat <- read.table(file2, header=TRUE, sep=";", ...)
# convert time-stamp:
if(todate)
  if(!any(c("Datum","Monat") %in% colnames(dat)))
    twarning("There is no column 'Datum' or 'Monat' in ",file) else
    {
    if(is.null(dat$Datum))
    dat$Monat <- as.Date(paste0(dat$Monat, "15"), format="%Y%m%d") else
    dat$Datum <- as.Date(as.character(dat$Datum), format="%Y%m%d")
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
#' # Temperature aggregates (2019-04 the 9th file, 2022-05 the 8th):
#' durl <- selectDWD(res="multi_annual", per="mean_81-10")[8]
#' murl <- selectDWD(res="multi_annual", per="mean_81-10", meta=TRUE)[8]
#' ma_temp <- dataDWD(durl, fileEncoding="")
#' ma_meta <- dataDWD(murl, fileEncoding="")
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
#' DEU <- terra::vect(system.file("extdata/DEU.gpkg", package="rdwd"))
#' pdf("MultiAnn.pdf", width=8, height=10)
#' par(bg="grey90")
#' for(m in colnames(ma)[8:19])
#'   {
#'   terra::plot(DEU, border="grey40")
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
#'              DEFAULT: "latin1"
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
#' link <- selectDWD(res="subdaily", var="standard_format", per="r")
#' link <- link[grepl("10381", link, fixed=TRUE)]
#' # Not ID, according to meta data, hence no longer in column id (2023-04).
#' file <- dataDWD(link, dir=locdir(), read=FALSE)
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
if(any(!has)) tstop("formIndex must contain column(s) ", musthave[!has])
# get column widths:
width <- diff(as.numeric(formIndex$Pos))
width <- c(width, 1)
# read fixed width dataset:
if(fast)
  {
  checkSuggestedPackage("readr", "readDWD.stand with fast=TRUE")
  coltypes <- readr::cols(X22=readr::col_character()) # 22 is S column
  # if not specified, will guess logical and then complain about character input later in file
  sf <- readr::read_fwf(file, readr::fwf_widths(width), col_types=coltypes, progress=FALSE)
  sf <- data.frame(sf) # loose tibble attributes and printing methods
  # mimick read.fwf behaviour:
  sf[is.na(sf)] <- " " # to avoid NA comparison
  for(i in c(2,4,5,6)) sf[,i] <- as.integer(sf[,i])
  } else # see developmentNotes for speed comparison
  sf <- read.fwf(file, widths=width, stringsAsFactors=FALSE, fileEncoding=fileEncoding, ...)
# dimension check:
if(ncol(sf) != nrow(formIndex)) tstop("incorrectly read file: ", file,"\n",
   ncol(sf), " columns instead of ", nrow(formIndex), " as dictated by formIndex.")
# NAs (starting with column 7):
for(i in which(formIndex$Fehlk!=""))
  {
  isNA <- as.character(sf[,i])==formIndex$Fehlk[i]
  if(anyNA(isNA)) tstop("NAs in comparison in column ", i, " of file ", file)
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
#' link <- link[!grepl("mn4", link)] # for mn4 file May 2022
#' link <- grep(".txt$", link, value=TRUE)
#' if(length(link)!=1) stop("length of link should be 1, but is ", length(link),
#'                 ":\n", berryFunctions::truncMessage(link,prefix="",sep="\n"))
#' 
#' file <- dataDWD(link, dir=locdir(), read=FALSE)
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
oneline <- sub("Berlin-Dahlem (LFAG)", "Berlin-Dahlem_(LFAG)", oneline, fixed=TRUE)
oneline <- sub("Berlin-Dahlem (FU)"  , "Berlin-Dahlem_(FU)"  , oneline, fixed=TRUE)
# Fix reading error if only these two exist, like in daily/kl/hist mn4_Beschreibung_Stationen.txt 2022-04-07

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
if(ncol(stats)!=8) tstop(ncol(stats)," columns detected instead of 8 for ", file)
classes <- c("integer", "integer", "integer", "integer", "numeric", "numeric", "character", "character")
actual <- sapply(stats, class)
if(actual[4]=="numeric") classes[4] <- "numeric"
if(!all(actual == classes))
  {
  msg <- paste0(names(actual)[actual!=classes], ": ", actual[actual!=classes],
                " instead of ", classes[actual!=classes], ".")
  msg <- paste(msg, collapse=" ")
  twarning("reading file '", file,"' did not give the correct column classes. ", msg)
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
#'                      dir=locdir(), read=FALSE)
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
#'                   dir=locdir(), read=FALSE)
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
#'                  to a list with dat ([`terra::rast`]) +
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
checkSuggestedPackage("terra", "rdwd:::readDWD.binary with toraster=TRUE")
pmessage("Converting to terra raster...")
rbmat <- base::lapply(rb,"[[",1)
rbmat <- lapply(rbmat, terra::rast)
rbmat <- terra::rast(rbmat)
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
#' @return [`terra::rast`] object
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018
#' @seealso [readDWD()]
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' rasterbase <- paste0(gridbase,"/seasonal/air_temperature_mean")
#' ftp.files <- indexFTP("/16_DJF", base=rasterbase, dir=tempdir())
#' localfiles <- dataDWD(ftp.files[1:2], base=rasterbase, joinbf=TRUE,
#'                       dir=locdir(), read=FALSE)
#' rf <- readDWD(localfiles[1])
#' rf <- readDWD(localfiles[1]) # runs faster at second time due to skip=TRUE
#' terra::plot(rf)
#' 
#' plotRadar(rf, proj="seasonal", extent=NULL)
#' 
#' testthat::expect_equal(c(terra::minmax(rf)), c(-8.2,4.4))
#' rf10 <- readDWD(localfiles[1], dividebyten=FALSE)
#' terra::plot(rf10)
#' testthat::expect_equal(c(terra::minmax(rf10*1)), c(-82,44))
#' }
#' @param file        Name of file on harddrive, like e.g.
#'                    DWDdata/grids_germany/seasonal/air_temperature_mean/
#'                    16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' @param gargs       Named list of arguments passed to [R.utils::gunzip()].
#'                    The internal defaults are: `remove=FALSE` (recommended to
#'                    keep this so `file` does not get deleted) and `skip=TRUE`
#'                    (which reads previously unzipped files as is).
#'                    If `file` has changed, use `gargs=list(temporary=TRUE)`.
#'                    The `gunzip` default `destname` means that the
#'                    unzipped file is stored at the same path as `file`.
#'                    DEFAULT gargs: NULL
#' @param dividebyten Logical: Divide the numerical values by 10? See [readDWD].
#'                    DEFAULT: TRUE
#' @param quiet       Ignored.
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots       Further arguments passed to [terra::rast()]
readDWD.raster <- function(file, gargs=NULL, dividebyten, quiet=rdwdquiet(), ...)
{
checkSuggestedPackage("R.utils", "rdwd:::readDWD.raster")
checkSuggestedPackage("terra",  "rdwd:::readDWD.raster")
#https://stackoverflow.com/questions/5227444/recursively-ftp-download-then-extract-gz-files
# gunzip arguments:
gdef <- list(filename=file, remove=FALSE, skip=TRUE)
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
rdata <- do.call(R.utils::gunzip, gfinal)
# raster reading:
r <- terra::rast(rdata, ...)
if(dividebyten) r <- r/10
return(invisible(r))
}



# ~ nc ----

#' @title read dwd netcdf data
#' @description Read netcdf data.
#' Intended to be called via [readDWD()].\cr
#' Note that `R.utils` and `ncdf4` must be installed to unzip and read the .nc.gz files.
#' @return [terra::rast()] object. Alternatively,
#' if toraster=FALSE, a list with time, lat, lon, var, varname, file and cdf.
#' **cdf** is the output of [ncdf4::nc_open()].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso [readDWD.hyras()] for non-packed .nc files, [readDWD()] 
#' @importFrom utils menu tail capture.output
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' 
#' library(berryFunctions) # for seqPal and colPointsLegend
#' 
#' url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
#' url <- "daily/Project_TRY/humidity/RH_199509_daymean.nc.gz"    # 25 MB
#' file <- dataDWD(url, base=gridbase, joinbf=TRUE, dir=locdir(), read=FALSE)
#' nc <- readDWD(file)
#' ncp <- plotRadar(nc, main=paste(terra::longnames(nc), terra::time(nc)), layer=1:3,
#'                  col=seqPal(), proj="nc", extent="nc")
#' 
#' str(terra::values(nc[[1]])) # obtain actual values into memory
#' 
#' terra::plot(nc[[1]]) # axes 0:938 / 0:720, the number of grid cells
#' terra::plot(ncp[[1]])# properly projected, per default onto latlon
#' 
#' rng <- range(terra::global(nc[[1:6]], "range", na.rm=TRUE))
#' terra::plot(nc, col=seqPal(), zlim=rng, maxnl=6)
#' 
#' # Array instead of terra rast:
#' nc <- readDWD(file, toraster=FALSE)
#' image(nc$var[,,1], col=seqPal(), asp=1.1)
#' colPointsLegend(nc$var[,,1], title=paste(nc$varname, nc$time[1]))
#' 
#' # interactive selection of variable:
#' # nc <- readDWD(file, toraster=FALSE, var="-") # commented out to not block automated tests
#' str(nc$var)
#' }
#' @param file        Name of file on harddrive, like e.g.
#'                    DWDdata/grids_germany/daily/Project_TRY/humidity/RH_199509_daymean.nc.gz
#' @param gargs       Named list of arguments passed to
#'                    [R.utils::gunzip()], see [readDWD.raster()]. DEFAULT: NULL
#' @param toraster    Read file with [terra::rast()]?
#'                    All further arguments will be ignored. Specify e.g. `var` 
#'                    through \dots as `varname`. DEFAULT: TRUE
#' @param var         if toraster=FALSE: Charstring with name of variable to be read with
#'                    [ncdf4::ncvar_get()]. If not available,
#'                    an interactive selection is presented.
#'                    DEFAULT: "" (last variable)
#' @param quiet       Logical: Suppress time conversion failure warning?
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots       Further arguments passed to [terra::rast()] or [ncdf4::nc_open()]
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
  checkSuggestedPackage("terra", "rdwd:::readDWD.nc with toraster=TRUE")
  # ignore ncdf print, see https://github.com/rspatial/raster/issues/143
  capture.output(out <- terra::rast(ncfile, ...))
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
 start <- strsplit(trimws(start), " ")[[1]][1]
 start <- as.Date(start)
 time <- start + ncdf4::ncvar_get(mycdf,'time')
 } else
 {
 start <- trimws(sub("hours since", "", unit)) # always hours?
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



# ~ hyras ----

#' @title read dwd hyras netcdf data
#' @description Read hyras netcdf data.
#' Intended to be called via [readDWD()].\cr
#' Note that `terra` must be installed to read the .nc files.
#' @return [terra::rast()] object.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2023
#' @seealso [readDWD.nc()] for packed .nc.gz files, [readDWD()]
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' link <- "monthly/hyras_de/humidity/hurs_hyras_5_2020_v5-0_de_monmean.nc"
#' hyras <- dataDWD(link, gridbase, joinbf=TRUE) # 0.9MB
#' plotRadar(hyras, proj="nc", extent=NULL, 
#'           main=substr(terra::time(hyras),1,7), layer=1:2)
#' }
#' @param file        Name of file on harddrive, like e.g.
#'                    DWDdata/monthly_hyras_de_humidity_hurs_hyras_5_2020_v5-0_de_monmean.nc
#' @param quiet       Currently not used.
#'                    DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots       Further arguments passed to [terra::rast()]
#' 
readDWD.hyras <- function(file, quiet=rdwdquiet(), ...)
{
checkSuggestedPackage("terra", "rdwd:::readDWD.hyras")
out <- terra::rast(file, ...)
return(invisible(out))
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
#' plotRadar(r$dat, main=paste("mm in 24 hours preceding", r$meta$date))
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  DWDdata/hourly/radolan/recent/bin/
#'                  raa01-rw_10000-1802020250-dwd---bin.gz
#' @param gargs     Named list of arguments passed to
#'                  [R.utils::gunzip()], see [readDWD.raster()]. DEFAULT: NULL
#' @param toraster  Logical: convert output (list of matrixes + meta informations)
#'                  to a list with data ([`terra::rast`]) +
#'                  meta (list from the first subfile, but with vector of dates)?
#'                  DEFAULT: TRUE
#' @param dividebyten Logical: Divide the numerical values by 10? See [readDWD].
#'                  toraster???  DEFAULT: TRUE
#' @param quiet     Ignored.
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots     Further arguments passed to [dwdradar::readRadarFile()],
#'                  i.e. `na` and `clutter`
readDWD.radar <- function(file, gargs=NULL, toraster=TRUE, dividebyten=TRUE, quiet=rdwdquiet(), ...)
{
checkSuggestedPackage("dwdradar","rdwd:::readDWD.radar")
checkSuggestedPackage("R.utils", "rdwd:::readDWD.radar")
# gunzip arguments:
gdef <- list(filename=file, remove=FALSE, skip=TRUE)
gfinal <- berryFunctions::owa(gdef, gargs, "filename")
rdata <- do.call(R.utils::gunzip, gfinal)
rf <- dwdradar::readRadarFile(rdata, ...)
if(toraster) checkSuggestedPackage("terra", "rdwd:::readDWD.radar with toraster=TRUE")
if(toraster) rf$dat <- terra::rast(rf$dat)
if(dividebyten) rf$dat <- rf$dat/10
return(invisible(rf))
}



# ~ asc ----

#' @title read dwd gridded radolan asc data
#' @description read grid-interpolated radolan asc data.
#' Intended to be called via [readDWD()].\cr
#' All layers (following `selection` if given) in all .tar.gz files are
#' combined into a terra raster with [terra::rast()].\cr
#' To project the data, use [projectRasterDWD()]
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, April 2019
#' @seealso [readDWD()]
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' # File selection and download:
#' radbase <- paste0(gridbase,"/hourly/radolan/historical/asc/")
#' radfile <- "2018/RW-201809.tar" # 25 MB to download
#' file <- dataDWD(radfile, base=radbase, joinbf=TRUE, read=FALSE)
#' #asc <- readDWD(file) # 4 GB in mem. ~ 20 secs unzip, 10 secs read, 2 min divide
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
#' plot(unlist(asc[800,800,]), type="l", xlab="Time [hours]")
#' 
#' # if dividebyten=FALSE, terra stores things out of memory in the exdir.
#' # by default, this is in tempdir, hence you would need to save asc manually:
#' # terra::writeRaster(asc, tempfile(fileext="/RW2018-09.gpkg"), overwrite=TRUE)
#' }
#' @param file        Name of file on harddrive, like e.g.
#'                    DWDdata/grids_germany/hourly/radolan/historical/asc/
#'                    2018_RW-201809.tar.
#'                    Must have been downloaded with `mode="wb"`!
#' @param exdir       Directory to unzip into. Unpacked files existing therein
#'                    will not be untarred again, saving up to 15 secs per file.
#'                    DEFAULT: NULL (subfolder of [tempdir()])
#' @param dividebyten Divide numerical values by 10? See [readDWD].
#'                    If dividebyten=FALSE and exdir left at NULL (tempdir), save
#'                    the result on disc with [terra::writeRaster()].
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
#' @param \dots       Further arguments passed to [terra::rast()]
readDWD.asc <- function(file, exdir=NULL, dividebyten=TRUE,
                        selection=NULL, quiet=rdwdquiet(), progbar=!quiet, ...)
{
checkSuggestedPackage("terra", "rdwd:::readDWD.asc")
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
# slow variant:
# dat <- terra::rast(f)
# if(!quiet & dividebyten) message("Dividing values by ten...")
# if(dividebyten) dat <- dat/10 # this loads it into memory and then takes 10 minutes
dat <- lapply(f, terra::rast, ...)
if(!quiet & dividebyten) message("Dividing values by ten...")
if(dividebyten) dat <- lapply(dat, function(x) x/10) # this takes 2 minutes
dat <- terra::rast(dat)
# output:
return(invisible(dat))
}



# ~ rklim ----

#' @title read dwd gridded radklim binary data
#' @description read gridded radklim binary data.
#' Intended to be called via [readDWD()].\cr
#' Note: needs dwdradar >= 0.2.6 (2021-08-08)
#' @return list depending on argument `toraster`, see there for details
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2021.
#' @seealso [readDWD.binary()], radar locations from \url{https://www.dwd.de/DE/leistungen/radarklimatologie/radklim_kompositformat_1_0.pdf?__blob=publicationFile&v=1}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' yw_link <- "/5_minutes/radolan/reproc/2017_002/bin/2022/YW2017.002_202203.tar"
#' # 202006 has untar error on Mac, 2023-04, maybe due to incomplete download
#' yw_file <- dataDWD(url=yw_link, base=gridbase, joinbf=TRUE, read=FALSE) # 207 MB
#' x <- readDWD(yw_file, selection=3641:3644)
#' # 00:30 for tar files, 01:40 for unpacking. 
#' # If you need a preselection argument, let me know.
#' terra::plot(x$dat)
#' plotRadar(x$dat[[1]], extent="rw") # better extent below
#'
#' f <- system.file("tests//raa01-yw2017.002_10000-2006131525-dwd---bin", package="dwdradar")
#' # https://stackoverflow.com/a/72207233/1587132 on how to install with tests folder
#' if(!file.exists(f)){
#' # Clone from https://github.com/brry/dwdradar:
#' f <- locdir(file="binary_testfile")
#' download.file(paste0("https://github.com/brry/dwdradar/raw/master/tests/",
#'                      "raa01-yw2017.002_10000-2006131525-dwd---bin"), f, mode="wb")
#' }
#' x <- dwdradar::readRadarFile(f)
#' x$dat <- terra::rast(x$dat)
#' terra::plot(x$dat)
#' plotRadar(x$dat, extent=c(-360, 380, -4730 ,-3690))
#' 
#' radloc <- read.table(header=T, sep=",", text="
#' ND, NM, NS  ,   ED, EM, ES
#' 53, 33, 50.4,   06, 44, 53.9
#' 51, 07, 26.5,   13, 45, 48.5
#' 51, 24, 18.5,   06, 57, 49.8
#' 47, 52, 21.3,   08, 00, 24.6
#' 54, 10, 23.2,   12, 06, 25.3
#' 52, 28, 40.3,   13, 23, 13.0
#' 54, 00, 15.8,   10, 02, 48.7
#' 51, 07, 28.7,   13, 46, 07.1
#' 49, 32, 26.4,   12, 24, 10.0
#' 53, 20, 19.4,   07, 01, 25.5
#' 51, 24, 20.2,   06, 58, 01.6
#' 47, 52, 25.0,   08, 00, 13.0
#' 51, 20, 06.0,   08, 51, 09.0
#' 51, 18, 40.3,   08, 48, 07.2
#' 50, 03, 06.0,   08, 34, 05.0
#' 50, 01, 20.8,   08, 33, 30.7
#' 53, 37, 16.5,   09, 59, 47.6
#' 52, 27, 47.0,   09, 41, 53.9
#' 52, 27, 36.2,   09, 41, 40.2
#' 48, 10, 28.9,   12, 06, 06.3
#' 48, 02, 31.7,   10, 13, 09.2
#' 48, 20, 10.9,   11, 36, 42.1
#' 50, 30, 00.4,   11, 08, 06.2
#' 50, 06, 34.7,   06, 32, 53.9
#' 49, 59, 05.1,   08, 42, 46.6
#' 52, 38, 55.2,   13, 51, 29.6
#' 54, 10, 32.4,   12, 03, 29.1
#' 48, 35, 07.0,   09, 46, 58.0
#' 52, 09, 36.3,   11, 10, 33.9")
#' radloc$x <- radloc$ED + radloc$EM/60 + radloc$ES/3600
#' radloc$y <- radloc$ND + radloc$NM/60 + radloc$NS/3600
#' for(i in 1:29) berryFunctions::circle(radloc$x[i], radloc$y[i], 0.9)
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  DWDdata/5_minutes_radolan_reproc_2017_002_bin_2020_YW2017.002_202006.tar
#' @param exdir     Directory to unzip into. If existing, only the needed files
#'                  will be unpacked with [untar()]. Note that exdir
#'                  size will be around 17 GB for 5-minute files.
#'                  If `unpacked=FALSE`, exdir can contain other files
#'                  that will be ignored for the actual reading.
#'                  DEFAULT: basename(file) at tempdir
#' @param unpacked  Manually indicate whether .tar.gz files within .tar file
#'                  have already been unpacked before.
#'                  DEFAULT: NULL: checks if 'yw.*--bin' file(s) are present
#' @param selection Optionally read only a subset of the ~ 12 x 24 x 30/31 = 8640 files.
#'                  Called as `f[selection]`. DEFAULT: NULL (ignored)
#' @param toraster  Logical: convert to [terra::rast]? see [readDWD.binary]
#'                  DEFAULT: TRUE
#' @param quiet     Suppress progress messages?
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param progbar   Show progress bars?
#'                  DEFAULT: !quiet, i.e. TRUE
#' @param \dots     Further arguments passed to [dwdradar::readRadarFile()],
#'                  i.e. `na` and `clutter`

readDWD.rklim <- function(file, exdir=NULL, unpacked=NULL, selection=NULL,
                            toraster=TRUE, quiet=rdwdquiet(), progbar=!quiet, ...)
{
checkSuggestedPackage("dwdradar", "rdwd:::readDWD.radklim")
if(progbar) lapply <- pbapply::pblapply
# exdir:
fn <- tools::file_path_sans_ext(basename(file))
if(is.null(exdir)) exdir <- paste0(tempdir(),"/", fn)
#
if(is.null(unpacked)){
unpacked <- any(grepl("yw.*--bin", dir(exdir)))
if(!quiet) if(unpacked) message("Assuming files have been unpacked.") else
                        message("Assuming files need to be unpacked.")
}

if(!unpacked){
# untar layer 1:
daydir <- paste0(exdir,"/dayfiles")
untar(file, exdir=daydir) # 30/31 .tar.gz files (one for each day). overwrites existing files
dayfiles <- dir(daydir, full.names=TRUE)
# untar layer 2:
if(!quiet) message("Listing contents of ",length(dayfiles)," .tar files ...")
original <- lapply(dayfiles, untar, list=TRUE) # needed for dir file selection
if(!quiet) message("Unpacking .tar files into ",exdir,"...")
lapply(dayfiles, untar, exdir=exdir)
# yields 30 * 24 * 12 = 8640 files each 1.7MB, takes ~25 secs
f <- dir(exdir, full.names=TRUE) # 8641 files (including "dayfiles" folder name)
# use only the files from file, not other stuff at exdir:
f <- f[basename(f) %in% unlist(original)]
unlink(daydir, recursive=TRUE)
} else
{
message("Assuming exdir ",exdir," \n         only contains binary files from file '",file,"'")
f <- dir(exdir, full.names=TRUE)
}

# read data (e.g 5-minutely files):
if(!is.null(selection)) f <- f[selection]
if(!quiet) message("Reading ",length(f)," files...")
rb <- lapply(f, dwdradar::readRadarFile, ...)

# list element names (time stamp):
time <- sapply(rb, function(x) format(x$meta$date, "%F %T"))
names(rb) <- time

if(!toraster) return(invisible(rb))
# else if toraster:
checkSuggestedPackage("terra", "rdwd:::readDWD.radklim with toraster=TRUE")
if(!quiet) message("Converting to terra raster...")
rbmat <- base::lapply(rb,"[[",1)
rbmat <- lapply(rbmat, terra::rast)
rbmat <- terra::rast(rbmat)
rbmeta <- rb[[1]]$meta
rbmeta$filename <- file
rbmeta$date <- as.POSIXct(time)
return(invisible(list(dat=rbmat, meta=rbmeta)))
}



# ~ grib2 ----

#' @title read nwp forecast data
#' @description read gridded numerical weather prediction data.
#' Intended to be called via [readDWD()].\cr
#' @return terra or stars object, depending on `pack`
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jan 2021.
#' @seealso [readDWD()]\cr
#' <https://www.dwd.de/EN/ourservices/nwp_forecast_data/nwp_forecast_data.html>\cr
#' <https://www.dwd.de/EN/aboutus/it/functions/Teasergroup/grib.html>\cr
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' nwp_t2m_base <- "ftp://opendata.dwd.de/weather/nwp/icon-d2/grib/15/soiltyp"
#' nwp_urls <- indexFTP("", base=nwp_t2m_base, dir=tempdir())
#' # for p instead of soiltyp, icosahedral_model-level files fail with GDAL errors,
#' # see https://github.com/brry/rdwd/issues/28
#' # regular-lat-lon_pressure-level files work with pack="terra" or "stars"
#' 
#' nwp_file <- dataDWD(tail(nwp_urls,1), base=nwp_t2m_base, dir=tempdir(), 
#'                     joinbf=TRUE, dbin=TRUE, read=FALSE)
#' nwp_data <- readDWD(nwp_file)
#' terra::plot(nwp_data)
#' addBorders() # the projection seems to be perfectly good :)
#' 
#' # index of GRIB files
#' if(FALSE){ # indexing takes about 6 minutes!
#' grib_base <- "ftp://opendata.dwd.de/weather/nwp/icon-d2/grib"
#' grib_files <- indexFTP("", base=grib_base, dir=tempdir())
#' for(f in unique(substr(grib_files, 1,3))) print(grib_files[which(substr(grib_files, 1,3)==f)[1]])
#' View(data.frame(grep("regular",grib_files, value=TRUE)))
#' }
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  cosmo-d2_germany_regular-lat-lon_single-level_2021010100_005_T_2M.grib2.bz2
#' @param pack      Char: package used for reading. 
#'                  One of "terra" or "stars".
#'                  "rgdal" (for the deprecated cosmo-d2 data) is no longer available,
#'                  see [issue](https://github.com/brry/rdwd/issues/28).
#'                  DEFAULT: "terra"
#' @param bargs     Named list of arguments passed to
#'                  [R.utils::bunzip2()], see `gargs` in [readDWD.raster()]. DEFAULT: NULL
#' @param quiet     Silence readGDAL completely, including warnings on 
#'                  discarded ellps / datum. 
#'                  DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots     Further arguments passed to [terra::rast()] or
#'                  [stars::read_stars()].
readDWD.grib2 <- function(
file,
pack="terra",
bargs=NULL,
quiet=rdwdquiet(),
...)
{
checkSuggestedPackage("R.utils", "rdwd:::readDWD.grib2")
# bunzip arguments:
bdef <- list(filename=file, remove=FALSE, skip=TRUE)
bfinal <- berryFunctions::owa(bdef, bargs, "filename")
# unzip:
bdata <- do.call(R.utils::bunzip2, bfinal)

# actual reading:
# TERRA ---
if(pack=="terra"){
if(!quiet) message(" with pack='terra' ...")
checkSuggestedPackage("terra"  , "rdwd:::readDWD.grib2")
out <- terra::rast(bdata, ...)
} else
# STARS ---
if(pack=="stars"){
if(!quiet) message(" with pack='stars' ...")
checkSuggestedPackage("stars"  , "rdwd:::readDWD.grib2")
out <- stars::read_stars(bdata, quiet=quiet, ...)
} else
# WRONG pack ---
tstop("pack='",pack,"' is not a valid option.")
# Output:
return(invisible(out))
}



# ~ pdf ----

#' @title open pdf data
#' @description open pdf file. This leads to less failures in the new `meta=TRUE`
#' # system in [selectDWD()].\cr
#' Intended to be called via [readDWD()].\cr
#' @return [berryFunctions::openFile()] output
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2022.
#' @seealso [readDWD()]
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' link <- selectDWD(res="hourly", var="solar", per="r", meta=TRUE)
#' if(!any(endsWith(link,"pdf"))) stop("no pdf file here anymore") 
#' # hourly/sun no longer has a pdf file anymore 2023-09
#' link <- link[endsWith(link,"pdf")][1]
#' file <- dataDWD(link, read=FALSE)
#' readDWD(file)
#' }
#' @param file      Name of file on harddrive, like e.g.
#'                  monthly_kl_historical_DESCRIPTION_obsgermany_climate_monthly_kl_historical_en.pdf
#' @param quiet     Ignored. DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots     Further arguments passed to [berryFunctions::openFile()] and from there to [system2()]

readDWD.pdf <- function(file, quiet=rdwdquiet(), ...)
{
berryFunctions::openFile(file, ...)
}

