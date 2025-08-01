# DWD Daten mit R runterladen, Wetter und Klimadaten in R
# Deutscher Wetterdienst R Daten download Klimastationen
# Weather Data Germany download with R, Climate Data Germany
#
#' @title Download data from the DWD CDC FTP Server
#' @description Get climate data from the German Weather Service (DWD) FTP-server.
#' The desired dataset is downloaded into `dir`.
#' If `read=TRUE`, it is also read and processed.\cr
#' `dataDWD` handles vectors of URLs,
#' displays progress bars (if the package `pbapply` is available)
#' and by default does not re-download data already in `dir`
#' (but see argument `force` to update files).\cr
#' To solve "errors in download.file: cannot open URL", see
#' <https://bookdown.org/brry/rdwd/fileindex.html>.\cr
#' 
#' @return Presuming downloading and processing were successful:
#'         if `read=TRUE`, the desired dataset
#'         (as returned by [readDWD()]),
#'         otherwise the filename as saved on disc
#'         (may have "_n" appended in name, see [berryFunctions::newFilename()]).\cr
#'         If length(file)>1, the output is a list of outputs / vector of filenames.\cr
#'         The output is always invisible.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun-Oct 2016
#' @seealso [selectDWD()]. [readDWD()], [download.file()].\cr
#'          <https://bookdown.org/brry/rdwd>\cr
#'          Helpful for plotting: [berryFunctions::monthAxis()],
#'          see also [berryFunctions::climateGraph()]
#' @keywords data file
#' @importFrom utils tail download.file browseURL
#' @importFrom berryFunctions newFilename owa tmessage twarning tstop truncMessage
#' @importFrom pbapply pblapply
#' @importFrom stats runif
#' @export
#' @examples
#' \dontrun{ ## requires internet connection
#' # find FTP files for a given station name and file path:
#' link <- selectDWD("Fuerstenzell", res="hourly", var="wind", per="recent")
#' # download file:
#' fname <- dataDWD(link, dir=locdir(), read=FALSE) ; fname
#' # dlocdir() is the default directory to store files
#' # unless force=TRUE, already obtained files will not be downloaded again
#' 
#' # read and plot file:
#' wind <- readDWD(fname, varnames=TRUE) ; head(wind)
#' metafiles <- readMeta(fname)          ; str(metafiles, max.level=1)
#' column_names <- readVars(fname)       ; head(column_names)
#' 
#' plotDWD(wind, "F.Windgeschwindigkeit", main="DWD hourly wind Fuerstenzell", 
#'         ylab="Hourly Wind speed  [m/s]", lwd=1)
#' 
#' 
#' # current and historical files, keep historical in the overlap time period:
#' link <- selectDWD("Potsdam", res="daily", var="kl", per="hr"); link
#' potsdam <- dataDWD(link, dir=locdir(), hr=4)
#' plot(TMK~MESS_DATUM, data=tail(potsdam,1500), type="l")
#' 
#' 
#' # With many files (>>50), use sleep to avoid getting kicked off the FTP server
#' #links <- selectDWD(res="daily", var="solar")
#' #sol <- dataDWD(links, sleep=20) # random waiting time after download (0 to 20 secs)
#' 
#' # Real life examples can be found in the use cases section of the website:
#' # browseURL("https://bookdown.org/brry/rdwd")
#' }
#' 
#' @param url    Char (vector): complete file URL(s) (including base and filename.zip) 
#'               as returned by [selectDWD()]. Can be a vector with several FTP URLs.
#' @param base   Single char: base URL that will be removed from output file names.
#'               DEFAULT: [`dwdbase`]
#' @param joinbf Logical: paste `base` and `file url` together?
#'               Needed mostly for data at [`gridbase`].
#'               DEFAULT: FALSE (selectDWD returns complete URLs already)
#' @param dir    Char: Writeable directory name where to save the downloaded file.
#'               Created if not existent. DEFAULT: [locdir()]
#' @param force  Logical (vector): always download, even if the file already exists in `dir`?
#'               Use NA to force re-downloading files older than 24 hours.
#'               Use a numerical value to force after that amount of hours.
#'               Use something like `c(Inf, 24)` or `force=c(24*365, 6)`, for per="hr".
#'               Note: if `force` is not FALSE, the `overwrite` default is TRUE.
#'               DEFAULT: FALSE
#' @param overwrite Logical (vector): if force=TRUE, overwrite the existing file
#'               rather than append "_1"/"_2" etc to the filename? 
#'               DEFAULT: `!isFALSE(force)`, i.e. true when `force` is specified.
#' @param read   Logical: read the file(s) with [readDWD()]? If FALSE,
#'               only download is performed and the filename(s) returned. DEFAULT: TRUE
#' @param dbin   Logical: Download binary file, i.e. add `mode="wb"` to the
#'               [download.file()] call? 
#'               See [Website](https://bookdown.org/brry/rdwd/raster-data.html#binary-file-errors) 
#'               for details.
#'               DEFAULT: TRUE
#' @param method [download.file] `method`. Introduced in version 1.5.25 (2022-05-12)
#'               as triggered by <https://github.com/brry/rdwd/issues/34>.
#'               DEFAULT: `getOption("download.file.method")`
#' @param dfargs Named list of additional arguments passed to [download.file()]
#'               Note that mode="wb" is already passed if `dbin=TRUE`
#' @param sleep  Number. If not 0, a random number of seconds between 0 and
#'               `sleep` is passed to [Sys.sleep()] after each download
#'               to avoid getting kicked off the FTP-Server,
#'               see note in [indexFTP()]. DEFAULT: 0
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               Only works if the R package `pbapply` is available. DEFAULT: TRUE (!quiet)
#' @param browse Logical: open repository via [browseURL()] and
#'               return URL folder path? If TRUE, no data is downloaded.
#'               If file has several values, only unique folders will be opened.
#'               DEFAULT: FALSE
#' @param ntrunc Single integer: number of filenames printed in messages
#'               before they get truncated with message "(and xx more)". DEFAULT: 2
#' @param file   Deprecated since rdwd version 1.3.34, 2020-07-28.
#' @param quiet  Logical: suppress message about directory / filenames?
#'               DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots  Further arguments passed to [readDWD()],
#'               like `fread`, `varnames`, `hr`, etc.
#
dataDWD <- function(
url,
base=dwdbase,
joinbf=FALSE,
dir=locdir(),
force=FALSE,
overwrite=!isFALSE(force),
read=TRUE,
dbin=TRUE,
method=getOption("download.file.method"),
dfargs=NULL,
sleep=0,
progbar=!quiet,
browse=FALSE,
ntrunc=2,
file=NULL,
quiet=rdwdquiet(),
...
)
{
if(!is.null(file)) tstop("The argument 'file' has been renamed to 'url' with rdwd version 1.3.34, 2020-07-28")
if(!is.atomic(url)) tstop("url must be a vector, not a ", class(url))
if(!is.character(url)) tstop("url must be char, not ", class(url))
base <- sub("/$","",base) # remove accidental trailing slash
url <- sub("^/","",url) # remove accidental leading slash
if(joinbf)  url <- paste0(base,"/",url)
if(missing(progbar) & length(url)==1) progbar <- FALSE
if(any(url==""))
{
  tmessage("Removing ", sum(url==""), " empty element(s) from url vector.")
  url <- url[url!=""]
}
if(length(url)<1) tstop("The vector of urls to be downloaded is empty.")
# be safe from accidental vector input:
dir     <- dir[1]
progbar <- progbar[1]
sleep   <- sleep[1]
quiet   <- quiet[1]
read    <- read[1]
browse  <- browse[1]
#
# open URL path(s) in internet browser:
if(browse)
  {
  folders <- unique(dirname(url))
  sapply(folders, browseURL)
  return(folders)
  }
# create directory to store downloaded data
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
# output file name(s)
hbas <- sub("^ftp://","https://", base) # for https base
outfile <- gsub(paste0(base,"/"), "", url)
outfile <- gsub(paste0(hbas,"/"), "", outfile)
outfile <- gsub("/", "_", outfile)

# force=NA management
if(is.null(force)) tstop("'force' cannot be NULL. Must be TRUE, FALSE, NA or a number.")
force <- rep(force, length=length(outfile)) # recycle vector
fT <- sapply(force, isTRUE)
fF <- sapply(force, isFALSE)
if(any(fT)) force[fT] <- 0
if(any(fF)) force[fF] <- Inf
force[is.na(force)] <- 24
force <- difftime(Sys.time(), file.mtime(outfile), units="h") > force

dontdownload <- file.exists(outfile) & !force
if( any(dontdownload) & !quiet )
  {
  tmessage(sum(dontdownload), " file", if(sum(dontdownload)>1)"s",
          " already existing and not downloaded again: ",
          berryFunctions::truncMessage(outfile[dontdownload], ntrunc=ntrunc, prefix=""),
          "\nNow downloading ",sum(!dontdownload)," files...")
  }
outfile <- newFilename(outfile, quiet=quiet, ignore=dontdownload,
                       overwrite=overwrite, ntrunc=ntrunc, tellignore=FALSE)
# since berryFunctions 1.15.9 (2017-06-14), outfile is now an absolute path
# Optional progress bar:
if(progbar) lapply <- pbapply::pblapply
# ------------------------------------------------------------------------------
# loop over each filename
dl_results <- lapply(seq_along(url), function(i)
  if(!dontdownload[i])
  {
  # Actual file download:
  dfdefaults <- list(url=url[i], destfile=outfile[i], method=method, quiet=TRUE)
  if(dbin) dfdefaults <- c(dfdefaults, mode="wb")
  e <- try(suppressWarnings(do.call(download.file,
                         berryFunctions::owa(dfdefaults, dfargs))), silent=TRUE)
  # wait some time to avoid FTP bot recognition:
  if(sleep!=0) Sys.sleep(runif(n=1, min=0, max=sleep))
  return(e)
  })

# check for download errors:
iserror <- sapply(dl_results, inherits, "try-error")
if(any(iserror))
  {
  ne <- sum(iserror)
  msg <- paste0(ne, " Download", if(ne>1) "s have" else " has",
                " failed (out of ",length(iserror),").",
                if(read)" Setting read=FALSE.")
  read <- FALSE
  msg2 <- paste0("\n- download.file error",if(ne>1) "s",":\n")
  msg3 <- sapply(dl_results[iserror], function(e)attr(e,"condition")$message)
  msg3 <- berryFunctions::truncMessage(msg3, ntrunc=15, prefix="", midfix="", altnix="", sep="\n")
  if(any(!substr(url[iserror], 1, 4) %in% c("ftp:","http")))
     msg <- paste0(msg, "\n- dataDWD needs urls starting with 'ftp://' or 'https://'. ",
                    "You can use joinbf=TRUE for relative links.")
  if(grepl("cannot open URL", msg3) || grepl("Kann URL .* nicht", msg3))
     msg <- paste0(msg, "\n- If files have been renamed on the DWD server, ",
                    "see   https://bookdown.org/brry/rdwd/fileindex.html")
  msg <- paste0(msg, msg2, msg3)
  warning(msg, call.=FALSE)
  }
# ------------------------------------------------------------------------------
# Output: Read the file or outfile name:
output <- outfile
if(read) output <- readDWD(file=outfile, quiet=quiet, progbar=progbar, ...)
# output:
return(invisible(output))
}
