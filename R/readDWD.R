# read dwd data ----

#' Process data from the DWD CDC FTP Server
#' 
#' Read climate data that was downloaded with \code{\link{dataDWD}}.
#' The data is unzipped and subsequently, the file is read, processed and
#' returned as a data.frame.\cr
#' New users are advised to set \code{varnames=TRUE} to obtain more informative
#' column names.\cr
#' \code{readDWD} will call internal (but documented) functions depending on the
#' arguments \code{meta, binary, raster, multia, asc}. Not all arguments to
#' \code{readDWD} are used for all functions, e.g. \code{fread} is used only by
#' \code{\link{readDWD.data}} (for observational data), while \code{dividebyten} 
#' is used in \code{\link{readDWD.raster}} and \code{\link{readDWD.asc}}
#' (for interpolated gridded data).\cr
#' \code{file} can be a vector with several filenames. Most other arguments can
#' also be a vector and will be recycled to the length of \code{file}.
#' 
#' @return Invisible data.frame of the desired dataset, 
#'         or a named list of data.frames if length(file) > 1.
#'         \code{\link{readDWD.binary}} returns a vector, 
#'         \code{\link{readDWD.raster}} and \code{\link{readDWD.asc}} 
#'         return raster objects instead of data.frames.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul-Oct 2016, Winter 2018/19
#' @seealso \code{\link{dataDWD}}, \code{\link{readVars}}, 
#'          \code{\link{readMeta}}, \code{\link{selectDWD}}
#' @keywords file chron
#' @importFrom utils read.table unzip read.fwf untar write.table
#' @importFrom berryFunctions checkFile na9 traceCall l2df
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
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: TRUE
#' @param fread  Logical (vector): read fast? See \code{\link{readDWD.data}}.
#'               DEFAULT: FALSE (some users complain it doesn't work on their PC)
#' @param varnames Logical (vector): Expand column names? 
#'               See \code{\link{readDWD.data}}. DEFAULT: FALSE
#' @param format,tz Format and time zone of time stamps, see \code{\link{readDWD.data}}
#' @param dividebyten Logical (vector): Divide the values in raster files by ten?
#'               Used in \code{\link{readDWD.raster}} and \code{\link{readDWD.asc}}.
#'               DEFAULT: TRUE
#' @param meta   Logical (vector): is the \code{file} a meta file (Beschreibung.txt)?
#'               See \code{\link{readDWD.meta}}.
#'               DEFAULT: TRUE for each file ending in ".txt"
#' @param multia Logical (vector): is the \code{file} a multi_annual file?
#'               Overrides \code{meta}, so set to FALSE manually if 
#'               \code{\link{readDWD.meta}} needs to be called on a file ending
#'               with "Standort.txt". See \code{\link{readDWD.multia}}.
#'               DEFAULT: TRUE for each file ending in "Standort.txt"
#' @param binary Logical (vector): does the \code{file} contain binary files?
#'               See \code{\link{readDWD.binary}}.
#'               DEFAULT: TRUE for each file ending in ".tar.gz"
#' @param raster Logical (vector): does the \code{file} contain a raster file?
#'               See \code{\link{readDWD.raster}}.
#'               DEFAULT: TRUE for each file ending in ".asc.gz"
#' @param asc    Logical (vector): does the \code{file} contain asc files?
#'               See \code{\link{readDWD.asc}}.
#'               DEFAULT: TRUE for each file ending in ".tar"
#' @param \dots  Further arguments passed to the internal \code{readDWD.*} 
#'               functions and from those to the underlying reading functions
#'               documented in each internal function.
#' 
readDWD <- function(
file,
progbar=TRUE,
fread=FALSE,
varnames=FALSE,
format=NA,
tz="GMT",
dividebyten=TRUE,
meta=  grepl(        '.txt$', file),
multia=grepl('Standort.txt$', file),
binary=grepl(     '.tar.gz$', file),
raster=grepl(     '.asc.gz$', file),
asc=   grepl(        '.tar$', file),
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
  format      <- rep(format,      length.out=len)
  tz          <- rep(tz,          length.out=len)
  dividebyten <- rep(dividebyten, length.out=len)
  meta        <- rep(meta,        length.out=len)
  multia      <- rep(multia,      length.out=len)
  binary      <- rep(binary,      length.out=len)
  raster      <- rep(raster,      length.out=len)
  asc         <- rep(asc,         length.out=len) 
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
# if meta/binary/raster/multia:
if(meta[i])   return(readDWD.meta(  file[i], ...))
if(binary[i]) return(readDWD.binary(file[i], progbar=progbar, ...))
if(raster[i]) return(readDWD.raster(file[i], dividebyten=dividebyten[i], ...))
if(multia[i]) return(readDWD.multia(file[i], ...))
if(asc[i])    return(readDWD.asc(   file[i], progbar=progbar, dividebyten=dividebyten[i], ...))
# if data:
readDWD.data(file[i], fread=fread[i], varnames=varnames[i], 
             format=format[i], tz=tz[i], ...)
}) # lapply loop end
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}





# read observational data ----

#' @title read regular dwd data
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso \code{\link{readDWD}}
#' @param file     Name of file on harddrive, like e.g. 
#'                 DWDdata/daily_kl_recent_tageswerte_KL_03987_akt.zip
#' @param fread    Logical: read faster with \code{data.table::\link[data.table]{fread}}?
#'                 When reading many large historical files, speedup is significant.
#'                 NA can also be used, which means TRUE if data.table is available.
#'                 Keep \code{varnames=FALSE} for the speed gain!
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
#' @param \dots    Further arguments passed to \code{\link{read.table}} or 
#'                 \code{data.table::\link[data.table]{fread}}
readDWD.data <- function(file, fread=FALSE, varnames=FALSE, format=NA, tz="GMT", ...)
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



#' @title read dwd metadata (Beschreibung*.txt files)
#' @description read dwd metadata (Beschreibung*.txt files).
#'  Column widths for \code{\link{read.fwf}} are computed internally.\cr
#'  if(any(meta)), \code{\link{readDWD}} tries to set the locale to German 
#'  (to handle Umlaute correctly). It is hence not recommended to call
#'  \code{rdwd:::readDWD.meta} directly on a file!\cr
#'  Names can later be changed to ascii with 
#'  \code{berryFunctions::\link{convertUmlaut}}.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @seealso \code{\link{readDWD}}
#' @param file  Name of file on harddrive, like e.g. 
#'              DWDdata/daily_kl_recent_KL_Tageswerte_Beschreibung_Stationen.txt
#' @param \dots Further arguments passed to \code{\link{read.fwf}}
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



#' @title read multi_annual dwd data
#' @description All other observational data at \code{\link{dwdbase}} 
#' (\url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}) can be read
#' with \code{\link{readDWD.data}}, except for the multi_annual data.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2019
#' @seealso \code{\link{readDWD}}
#' @param file  Name of file on harddrive, like e.g. 
#'              DWDdata/multi_annual_mean_81-10_Temperatur_1981-2010_aktStandort.txt or
#'              DWDdata/multi_annual_mean_81-10_Temperatur_1981-2010_Stationsliste_aktStandort.txt
#' @param \dots Further arguments passed to \code{\link{read.table}}
readDWD.multia <- function(file, ...)
{
out <- read.table(file, sep=";", header=TRUE, ...)
nc <- ncol(out)
# presumably, all files have a trailing empty column...
if(colnames(out)[nc]=="X") out <- out[,-nc]
out
}



# read gridded data ----

#' @title read dwd gridded radolan binary data
#' @description This does not work correctly yet for the tested data 
#' (grids_germany/daily/radolan/historical)! Hints are welcome!
#' @return vector
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018
#' @seealso \code{\link{readDWD}}
#' @param file      Name of file on harddrive, like e.g. 
#'                  DWDdata/grids_germany_daily_radolan_historical_2017_SF201712.tar.gz
#' @param progbar   Show messages and progress bars? \code{\link{readDWD}} will
#'                  keep progbar=TRUE for binary files, even if length(file)==1.
#'                  DEFAULT: TRUE
#' @param selection Optionally read only a subset of the ~24*31=744 files.
#'                  Called as \code{f[selection]}. DEFAULT: NULL (ignored)
#' @param \dots     Further arguments passed to \code{\link{readBin}}
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




#' @title read dwd gridded raster data
#' @return \code{raster::\link[raster]{raster}} object
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Dec 2018
#' @seealso \code{\link{readDWD}}
#' @param file        Name of file on harddrive, like e.g. 
#'                    DWDdata/grids_germany/seasonal/air_temperature_mean/
#'                    16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' @param dividebyten Logical: Divide the numerical values by 10?
#'                    DEFAULT: TRUE
#' @param \dots       Further arguments passed to \code{raster::\link[raster]{raster}}
readDWD.raster <- function(file, dividebyten, ...)
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



#' @title read dwd gridded radolan asc data
#' @description read grid-interpolated radolan asc data. See 
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan/README.txt}
#' All layers (following \code{selection} if given) in all .tar.gz files are 
#' combined into a raster stack with \code{raster::\link[raster]{stack}}.
#' @return data.frame
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, April 2019
#' @seealso \code{\link{readDWD}}
#' @param file        Name of file on harddrive, like e.g. 
#'                    DWDdata/grids_germany/hourly/radolan/historical/asc/
#'                    2018_RW-201809.tar.
#'                    Must be downloaded with \code{mode="wb"}!
#' @param exdir       Directory to unzip into. Unpacked files existing therein
#'                    will not be untarred again, saving up to 15 secs per file.
#'                    DEFAULT: NULL (subfolder of \code{\link{tempdir}()})
#' @param dividebyten Divide numerical values by 10? If dividebyten=FALSE, save 
#'                    the result on disc with \code{raster::\link[raster]{writeRaster}}.
#'                    Accessing out-of-memory raster objects won't work if 
#'                    exdir is removed! -> Error in .local(.Object, ...)
#'                    DEFAULT: TRUE
#' @param setpe       Set projection and extent? DEFAULT: FALSE
#' @param proj        Projection set (if setpe) with \code{raster::\link[raster]{projection}}.
#'                    DEFAULT: NULL (internal value, provided 2019-04 by Antonia Hengst)
#' @param extent      Extent set (if setpe) with \code{raster::\link[raster]{extent}}.
#'                    DEFAULT: NULL (internal value, provided 2019-04 by Antonia Hengst)
#' @param progbar     Show messages and progress bars? \code{\link{readDWD}} will
#'                    keep progbar=TRUE for asc files, even if length(file)==1.
#'                    DEFAULT: TRUE
#' @param selection   Optionally read only a subset of the ~24*31=744 files.
#'                    Called as \code{hourfiles[selection]}. DEFAULT: NULL (ignored)
#' @param \dots       Further arguments passed to \code{raster::\link[raster]{raster}}
# @importFrom raster raster stack crs projection extent plot
#' @examples 
#' \dontrun{ # Excludedfrom CRAN checks
#' # File selection and download:
#' datadir <- paste0(berryFunctions::packagePath(), "/localtests/CreateVignettes/DWDdata")
#' radbase <-"ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/hourly/radolan/historical/asc/" 
#' radfile <- "2018/RW-201809.tar" # 25 MB to download
#' file <- dataDWD(paste0(radbase,"/",radfile), base=radbase, dir=datadir,
#'                 dfargs=list(mode="wb"), read=FALSE) # download with mode=wb!!!
#'                 
#' #asc <- readDWD(file) # 4 GB in mem. ~ 20 secs unzip, 30 secs read, 10 min divide
#' asc <- readDWD(file, selection=1:20, setpe=TRUE, dividebyten=FALSE)
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
#' # raster::writeRaster(asc, paste0(datadir,"/RW2018-09"), overwrite=TRUE) 
#' }
readDWD.asc <- function(file, exdir=NULL, dividebyten=TRUE, 
                        setpe=FALSE, proj=NULL, extent=NULL, 
                        selection=NULL, progbar=TRUE, ...)
{
if(!requireNamespace("raster", quietly=TRUE))
stop("To use rdwd:::readDWD.asc, please first install raster:",
     "   install.packages('raster')", call.=FALSE)
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
if(progbar) message("\nChecking if already unpacked: ", file, "...")
to_untar <- lapply(dayfiles, untar, list=TRUE)
untarred <- dir(exdir, pattern=".asc$")
to_untar <- !sapply(to_untar, function(x) all(x %in% untarred))
if(any(to_untar)){
  if(progbar) message("Unpacking tar files into ",exdir,"...")
  lapply(dayfiles[to_untar], untar, exdir=exdir) 
} else if(progbar) message("Tar file was already unpacked into ",exdir," :)")
# yields 31 * 24 .asc files each 1.7MB, takes ~20 secs
#
#
# read data:
hourfiles <- dir(exdir, pattern=".asc$", full.names=TRUE) # 720 files
if(!is.null(selection)) hourfiles <- hourfiles[selection]
if(progbar) message("Reading ",length(hourfiles)," files...")
dat <- lapply(hourfiles, raster::raster, ...)
#
# divide by ten (takes ~9 min!)
if(progbar & dividebyten) message("Dividing values by ten...")
if(dividebyten) dat <- lapply(dat, function(x) x/10)
#
# stack layers:
dat <- raster::stack(dat)
#
# set projection + extent:
if(setpe)
  {
  if(is.null(proj)) proj <- raster::crs("+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 
    +k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs")
  if(is.null(extent)) extent <- raster::extent(-523.4622,376.5378,-4658.645,-3758.645)
  raster::projection(dat) <- proj
  raster::extent(    dat) <- extent
  }
# output:
return(invisible(dat))
}
