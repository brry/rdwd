# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Weather Data Germany download with R, Climate Data Germany
# Deutscher Wetterdienst R Daten download Klimastationen

# For html rendered documentation, please visit
#   https://www.rdocumentation.org/packages/berryFunctions/versions/1.12.3/topics/dataDWD
#
#' download data from DWD
#'
#' Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired .zip dataset is downloaded into \code{dir}, read, processed and returned as a data.frame
#'
#' @return data.frame of the desired dataset (returned by \code{\link{readDWD}} if meta=0),
#'         presuming downloading and processing were successfull.
#'         Alternatively, links that were opened if \code{browse}!=0.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2016
#' @seealso \code{\link{readDWD}}, \code{\link{download.file}}.
#'          Helpful for plotting: \code{berryFunctions::\link[berryFunctions]{monthAxis}},
#'          see also \code{berryFunctions::\link[berryFunctions]{climateGraph}}
#' @keywords data file
#' @importFrom utils download.file read.fwf read.table unzip write.table
#' @export
#' @examples
#' \dontrun{ ## Not run in CRAN checks because of downloading, writing files, etc
#'
#' # 1. Basic usage ------------------------------------------------------------
#'
#' prec <- dataDWD(file="stundenwerte_RR_02787_akt.zip")
#' plot(prec$MESS_DATUM, prec$NIEDERSCHLAGSHOEHE, main="DWD hourly rain Kupferzell", col="blue",
#'      xaxt="n", las=1, type="l", xlab="Date", ylab="Hourly rainfall  [mm]")
#' monthAxis(1, ym=T)
#'
#' prec2 <- dataDWD("stundenwerte_RR_03987_akt.zip") # writes into the same folder
#'
#'
#' # 2. find certain station ---------------------------------------------------
#' # Get long term climate records of a certain station (e.g. Kirchberg)
#'
#' dataDWD("", browse=2, base2="monthly/kl/historical") # open link in browser
#' # metadata for all existing stations:
#' stats <- dataDWD("KL_Monatswerte_Beschreibung_Stationen.txt", base2="monthly/kl/historical")
#' str(stats)  # data.frame with 8 columns (4 int, 2 num, 2 factor), 1053 rows (July 2016)
#' stats[grep("kirchberg", stats$Stationsname, ignore.case=TRUE), ]
#' # identify the station id you need (there may be multiple matches): 02575
#'
#'
#' # List of actually available files (needs RCurl):
#' # install.packages("RCurl")
#' files <- dataDWD("", meta=2, base2="monthly/kl/historical")
#' #   files <- strsplit(files, "\n")[[1]]   # needed on linux
#' clim <- dataDWD(base2="monthly/kl/historical", file=files[grep("_02575_", files)])
#' # monthly averages/mins/maxs of: wind, clouds, rainfall, sunshine, temperature
#' head(clim)
#'
#' # Map of all precipitation stations:
#' if(FALSE){ # pdf saving works only in berryFunctions source directory
#' pstats <- dataDWD("RR_Stundenwerte_Beschreibung_Stationen.txt",
#'                   base2="hourly/precipitation/historical")
#' pfiles <- dataDWD("", meta=2, base2="hourly/precipitation/historical")
#' hasfile <- pstats$Stations_id %in% na.omit(as.numeric(substr(pfiles, 17, 21)))
#' library("OSMscale")
#' map <- pointsMap(geoBreite, geoLaenge, data=pstats, fx=0.28, fy=0.06)
#' pdf("inst/extdata/RainfallStationsMap.pdf")
#' plot(map)
#' scaleBar(map, x=0.05, y=0.03, abslen=200)
#' pp <- projectPoints(geoBreite, geoLaenge, data=pstats, to=posm())
#' points(pp[!hasfile,], col="red", pch=3)
#' points(pp[ hasfile,], col="blue", pch=3)
#' legend("bottomright", c("in matadata only", "file on FTP server"),
#'        col=c("red", "blue"), pch=3, bg="white")
#' title(main="DWD stations: Rainfall data on ftp server", line=3)
#' dev.off()
#' }
#'
#' # 3. Get data for several stations ------------------------------------------
#' # (do this at your own risk of getting kicked off the FTP)
#'
#' files <- dataDWD("", meta=2)
#' #   files <- strsplit(files, "\n")[[1]]   # needed on linux
#' headtail(sort(files),6)
#' # Apply the function to several files, create a list of data.frames:
#' files <- files[grep(".zip", files, fixed=TRUE)]
#' prec <- lapply(files[1:2], function(f) {Sys.sleep(runif(1,0,5)); dataDWD(f)})
#' names(prec) <- substr(files[1:2], 14, 21)
#' str(prec, max.level=1)
#'
#' # Real life example with data completeness check etc:
#' browseURL("http://github.com/brry/prectemp/blob/master/Code_example.R")
#'
#'
#' # 4. Test metadata part of function -----------------------------------------
#'
#' files <- read.table(as.is=TRUE, text="
#' #ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/
#' daily/kl/historical                KL_Tageswerte_Beschreibung_Stationen.txt
#' daily/kl/recent                    KL_Tageswerte_Beschreibung_Stationen.txt
#' daily/more_precip/historical       RR_Tageswerte_Beschreibung_Stationen.txt
#' daily/more_precip/recent           RR_Tageswerte_Beschreibung_Stationen.txt
#' daily/soil_temperature/historical  EB_Tageswerte_Beschreibung_Stationen.txt
#' daily/soil_temperature/recent      EB_Tageswerte_Beschreibung_Stationen.txt
#' daily/solar                        ST_Beschreibung_Stationen.txt
#' hourly/air_temperature/historical  TU_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/air_temperature/recent      TU_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/cloudiness/historical       N_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/cloudiness/recent           N_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/precipitation/historical    RR_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/precipitation/recent        RR_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/pressure/historical         P0_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/pressure/recent             P0_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/soil_temperature/historical EB_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/soil_temperature/recent     EB_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/solar                       ST_Beschreibung_Stationen.txt
#' hourly/sun/historical              SD_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/sun/recent                  SD_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/wind/historical             FF_Stundenwerte_Beschreibung_Stationen.txt
#' hourly/wind/recent                 FF_Stundenwerte_Beschreibung_Stationen.txt
#' monthly/kl/historical              KL_Monatswerte_Beschreibung_Stationen.txt
#' monthly/kl/recent                  KL_Monatswerte_Beschreibung_Stationen.txt
#' monthly/more_precip/historical     RR_Monatswerte_Beschreibung_Stationen.txt
#' monthly/more_precip/recent         RR_Monatswerte_Beschreibung_Stationen.txt")
#' i=1
#' meta <- dataDWD(file=files[i,2], base2=files[i,1])
#' colPoints(geoLaenge, geoBreite, Stations_id, data=meta, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, Stationshoehe, data=meta, add=F, asp=1.5)
#' meta$von_jahr <- meta$von_datum/1e4
#' meta$bis_jahr <- meta$bis_datum/1e4
#' meta$dauer <- meta$bis_jahr - meta$von_jahr
#' colPoints(geoLaenge, geoBreite, von_jahr, data=meta, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, bis_jahr, data=meta, add=F, asp=1.5)
#' colPoints(geoLaenge, geoBreite, dauer, data=meta, add=F, asp=1.5)
#' hist(meta$bis_jahr, breaks=50, col="purple")
#' hist(meta$dauer, breaks=50, col="purple")
#' sum(meta$dauer>50); mean(meta$dauer>50)
#' # 356 (32.7%) stations with more than 50 years of data (according to metadata)
#' }
#'
#' @param file Filename (must be available at the location given by \code{base1} and \code{base2})
#' @param base1 Main directory of DWD ftp server (can probably always be left unchanged)
#' @param base2 Characterstring with subdirectory. DEFAULT: "hourly/precipitation/recent"
#' @param dir Writeable directory on your computer. Created if not existent.
#'            DEFAULT: "DWDdata" at your current \code{\link{getwd}()}
#' @param browse Integer specifying whether and how to open repository via \code{\link{browseURL}}.\cr
#'               0 for regular file download. \cr
#'               1 to open \code{base1}.\cr
#'               2 to open \code{base1/base2}).\cr
#'               If base= 1 or 2, no \code{dir} is created and no download performed. DEFAULT: 0
#' @param meta Integer specifying whether to get metadata instead of actual data.\cr
#'               0 for regular file. \cr
#'               1 for meta data of all stations
#'               (\code{meta} is automatically set to 1 if \code{file} ends in ".txt".
#'               Column widths for \code{\link{read.fwf}} are computed internally).\cr
#'               2 for a list of the available files (requires \code{RCurl} to be installed.
#'               If meta=2, \code{file=""} is possible, as it is ignored anyways).\cr
#'               DEFAULT: 0
#' @param read Read the file with \code{\link{readDWD}}?
#'             If FALSE, only download is performed. DEFAULT: TRUE
#' @param format Format used in \code{\link{strptime}} to convert date/time column,
#'               see \code{\link{readDWD}}. DEFAULT: NA
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#' @param \dots Further arguments currently ignored
#'
dataDWD <- function(
file,
base1="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
base2="hourly/precipitation/recent",
dir="DWDdata",
browse=0:2,
meta=0:2,
read=TRUE,
format=NA,
quiet=FALSE,
...
)
{
browse <- browse[1]
meta <- meta[1]
# Open link in browser
if(browse==1)
  {
  browseURL(base1)
  return(base1)
  }
if(browse==2)
  {
  browseURL(paste0(base1,"/",base2))
  return(paste0(base1,"/",base2))
  }
#
# create directory to store original and processed data
if(!file.exists(dir))
  {
  dir.create(dir)
  if(!quiet) message("Directory '", dir, "' created at '", getwd(),"'")
  } else
  if(!quiet) message("Adding to directory '", dir, "' at '", getwd(),"'")
owd <- setwd(dir)
on.exit(setwd(owd))
#
# input checks:
if(length(file)!=1) stop("file must be a single character string")
if(substring(file, nchar(file)-3)==".txt") meta <- 1
#
link <- paste0(base1,"/",base2,"/",file)
#
# station info:
if(meta==1)
  {
  # one line to confirm widths and get column names
  widths <- c( 6,9,9,15,12,10,41,100)
  if(file=="") stop("dataDWD argument 'file' may not be empty if meta=1")
  oneline <- readLines(link, n=3)
  if(substr(oneline[3],1,6)=="      ") widths[1] <- 11
  # read metadata: suppress readLines warning about EOL
  stats <- suppressWarnings(read.fwf(link, widths=widths, skip=2, strip.white=TRUE) )
  colnames(stats) <- strsplit(oneline[1], " ")[[1]]
  if(file.exists(file)) warning("File '",file,"' already existed, is now overwritten.")
  write.table(stats, file=file, row.names=FALSE, quote=FALSE, sep="\t")
  return(stats)
  }
# list available files
if(meta==2)
  {
  if(!requireNamespace("RCurl", quietly=TRUE))
    stop("The R package 'Rcurl' is not available. dataDWD(..., meta=2) is not possible.
         install.packages('RCurl')    to enable this.")
  f <- RCurl::getURL(paste0(base1,"/",base2,"/"), verbose=F, ftp.use.epsv=TRUE, dirlistonly=TRUE)
  f <- strsplit(f, "\r\n")[[1]]
  file <- paste0("INDEX_of_DWD_", gsub("/","_",base2),".txt")
  if(file.exists(file)) warning("File '",file,"' already existed, is now overwritten.")
  write.table(f, file=file, row.names=FALSE, col.names=FALSE, quote=FALSE)
  return(f)
  }
#
# Regular file download:
download.file(url=link, destfile=file, quiet=TRUE)
if(read) readDWD(file=file, format=format)
}
