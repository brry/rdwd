#' @title run local tests of rdwd
#' @description Run \code{rdwd} tests on local machine. Due to time-intensive 
#' data downloads, these tests are not run automatically on CRAN.
#' @return Time taken to run tests in minutes
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr-Oct 2019
#' @seealso \code{\link{localtestdir}}
#' @keywords debugging
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par title
#' @export
#'
#' @param dir_data  Reusable data location. Preferably not under version control.
#'                  DEFAULT: \code{\link{localtestdir}()}
#' @param dir_exmpl Reusable example location. DEFAULT: localtestdir(folder="misc/ExampleTests")
#' @param fast      Exclude many tests? DEFAULT: FALSE
#' @param radar     Test reading radar example files. DEFAULT: !fast
#' @param all_Potsdam_files Read all (ca 60) files for Potsdam? Re-downloads if
#'              files are older than 24 hours. Reduce test time a lot by setting
#'              this to FALSE. DEFAULT: !fast
#' @param examples Run Examples (including donttest sections) DEFAULT: !fast
#' @param quiet Suppress progress messages? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#'
runLocalTests <- function(
dir_data=localtestdir(),
dir_exmpl=localtestdir(folder="misc/ExampleTests"),
fast=FALSE,              # ca 0.1 minutes (always, even if fast=T)
radar=!fast,             # ca 0.3 minutes
all_Potsdam_files=!fast, # ca 1.6 minutes
examples=!fast,          # ca 2.1 minutes
quiet=rdwdquiet()
)
{
# pre-checks ----
checkSuggestedPackage("testthat", "runLocalTests")
if(!grepl("rdwd$", getwd())) 
  warning("getwd must be in package root folder.", immediate.=TRUE)
begintime <- Sys.time()
messaget <- function(x) if(!quiet) message(x, " (",
          round(difftime(Sys.time(), begintime, units="s")), " secs so far)")
#
# readDWD.data ----

messaget("++ Testing dataDWD + readDWD.data")

testthat::test_that("dataDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=dir_data, quiet=TRUE)
testthat::expect_equal(basename(file), "daily_kl_recent_tageswerte_KL_03987_akt.zip")
links <- selectDWD(id=c(5302,5711,6295),res="daily",var="more_precip",per="h")
testthat::expect_error(dataDWD(links, dir=dir_data), "file must be a vector, not a list")
testthat::expect_warning(dataDWD("multi/mean/Temp.txt", quiet=TRUE), 
               "dataDWD needs urls starting with 'ftp://'.")
})

testthat::test_that("readDWD.data works for regular data", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=dir_data, quiet=TRUE)
clim <- readDWD(file)
supposedcolnames <- c("STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM", "QN_4", 
                      "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", 
                      "UPM", "TXK", "TNK", "TGK", "eor")
testthat::expect_equal(colnames(clim), supposedcolnames)
climf <- readDWD(file, fread=TRUE)
testthat::expect_equal(clim, climf)
#
clim_vn  <- readDWD(file, varnames=TRUE)
clim_vnf <- readDWD(file, varnames=TRUE, fread=TRUE)
testthat::expect_equivalent(clim, clim_vn)
testthat::expect_equal(clim_vn, clim_vnf)
})

testthat::test_that("readDWD.data works for 10 minute data", {
link <- selectDWD("Kiel-Holtenau", res="10_minutes", var="air_temperature", per="recent") 
file <- dataDWD(link, read=FALSE, dir=dir_data)
air_temperature <- readDWD(file, varnames=TRUE)
time_diff <- as.numeric(diff(air_temperature$MESS_DATUM[1:10]))
testthat::expect_equal(time_diff, rep(10,9))
})


# readRadarFile ----

if(radar)
{
messaget("++ Testing readRadarFile")
#
if(!file.exists(dir_exmpl)) dir.create(dir_exmpl)
#
testthat::test_that("readRadarFile works", {
trr <- function(file, ext="radolan", readdwd=FALSE) # trr: test reading radar data
  {
  main <- deparse(substitute(file))
  file2 <- localtestdir(folder="misc", file=file)
  rrf <- if(readdwd) readDWD(file2, toraster=FALSE) else dwdradar::readRadarFile(file2)
  rrr <- raster::raster(rrf$dat)
  rrp <- projectRasterDWD(rrr, extent=ext)
  raster::plot(rrr, main="\nOriginal")
  raster::plot(rrp, main="\nProjected")
  addBorders()
  title(main=main, outer=TRUE, line=-1.1)
  rngr <- range(raster::cellStats(rrr, "range"))
  rngp <- range(raster::cellStats(rrp, "range"))
  return(list(file=file2, rrp=rrp, meta=rrf$meta, range_orig=rngr, range_proj=rngp))
  }
pdf(paste0(dir_exmpl,"/Radartests.pdf"), width=10, height=7)
par(mfrow=c(1,2), mar=c(2,2,3,3), mgp=c(2,0.7,0))
w1 <- trr("raa01-rw2017.002_10000-1712310850-dwd---bin_hourRadReproc", ext="rw")
w2 <- trr("raa01-rw_10000-1907311350-dwd---bin_hourRadRecentBin.gz", readdwd=TRUE)
rw <- trr("raa01-rw_10000-1907010950-dwd---bin_weatherRadolan")
sf <- trr("raa01-sf_10000-1605010450-dwd---bin_dailyRadHist")
rx <- trr("raa01-rx_10000-1605290600-dwd---bin_Braunsbach")
rx1 <- raster::raster(dwdradar::readRadarFile(rx$file)$dat)
rx2 <- projectRasterDWD(rx1, targetproj=NULL)
raster::plot(rx2, main="\nProjected without latlon")
raster::plot(rx$rrp, zlim=rx$range_orig, main="\nProjected, with custom zlim")
addBorders()
dev.off()
if(interactive()) berryFunctions::openFile(paste0(dir_exmpl,"/Radartests.pdf"))
#
# "True" values from versions of reading functions that seem to make sense.
# NOT actually checked with DWD, reality or anything!
#
rangecheck <- function(rr, orig, proj, tolerance=0.01)
  {
  name <- deparse(substitute(rr))
  rc <- function(is, should, msg)
  {
  eq <- berryFunctions::almost.equal(is, should, tolerance=tolerance, scale=1)
  if(any(!eq)) stop(msg, " not correct for: ", name, "\n", 
               toString(round(is,5)), "   instead of   ", toString(should), "\n")
  }
  rc(rr$range_orig, orig, "Range (unprojected)")
  rc(rr$range_proj, proj, "Range (projected)")
  }
rangecheck(w1, c( 0.0,  6.2), c( 0.00,  5.87))
rangecheck(w2, c( 0.0, 72.6), c(-0.19, 70.98))
rangecheck(rw, c( 0.0, 30.7), c(-0.45, 30.45))
rangecheck(sf, c( 0.0, 39.2), c(-0.03, 38.20))
rangecheck(rx, c(31.5, 95.0), c(18.30, 97.17))
})
} # End radar


# findID ----

messaget("++ Testing findID + selectDWD")

testthat::test_that("findID warns as wanted", {
testthat::expect_warning(findID("this_is_not_a_city"),
               "findID: no ID could be determined from name 'this_is_not_a_city'.")
testthat::expect_warning(findID(c("Wuppertal","this_is_not_a_city") ),
               "findID: no ID could be determined from name 'this_is_not_a_city'.")
testthat::expect_warning(findID(7777),
               "findID: no ID could be determined from name '7777'.")
testthat::expect_warning(findID("01050"),
               "findID: no ID could be determined from name '01050'.")
testthat::expect_equal(findID(), "")
})



# indexFTP----

testthat::test_that("indexFTP warns and works as intended", {
base <- "https://opendata.dwd.de/weather/radar/radolan/rw/"
testthat::expect_warning(indexFTP(base, folder="", dir=tempdir(), quiet=TRUE),
                         "base should start with ftp://")
base <- "ftp://ftp-cdc.dwd.de/weather/radar/radolan/rw"
rw <- indexFTP(base, folder="", dir=tempdir(), quiet=TRUE, exclude.latest.bin=FALSE)
testthat::expect_equal(tail(rw,1), "/raa01-rw_10000-latest-dwd---bin")
})



# selectDWD ----

testthat::test_that("selectDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
testthat::expect_equal(link, paste0(dwdbase,"/daily/kl/recent/tageswerte_KL_03987_akt.zip"))
testthat::expect_equal(selectDWD("Potsdam", res="daily", var="solar"),
             paste0(dwdbase,"/daily/solar/tageswerte_ST_03987_row.zip"))
})

testthat::test_that("selectDWD id input can be numeric or character", {
testthat::expect_equal(selectDWD(id="00386", res="daily", var="kl", per="historical"),
             selectDWD(id=386,     res="daily", var="kl", per="historical"))
})

testthat::test_that("selectDWD can choose Beschreibung meta files", {
testthat::expect_equal(selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE),
  paste0(dwdbase, "/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"))
  
testthat::expect_equal(selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE),
  selectDWD(res="daily", var="kl", per="h", meta=TRUE))
})


testthat::test_that("selectDWD properly vectorizes", {
testthat::expect_type(selectDWD(id="01050", res="daily", var="kl", per=c("r","h")), "list")
testthat::expect_type(selectDWD(id="01050", res="daily", var="kl", per="rh"), "character")
# all zip files in all paths matching id:
allzip_id <- selectDWD(id=c(1050, 386), res="",var="",per="")
# all zip files in a given path (if ID is empty):
allzip_folder <- selectDWD(id="", res="daily", var="kl", per="recent") 
testthat::expect_equal(length(allzip_id), 2)
testthat::expect_gte(length(allzip_id[[1]]), 200)
testthat::expect_gte(length(allzip_id[[2]]), 7)
testthat::expect_gte(length(allzip_folder), 573)
})


# selectDWD warnings ----

messaget("++ Testing selectDWD warnings")

testthat::test_that("selectDWD warns as intended", {
testthat::expect_warning(selectDWD(res="",var="",per=""), 
               "selectDWD: neither station ID nor valid FTP folder is given.")
testthat::expect_warning(selectDWD(7777, res="",var="",per=""),
               "selectDWD -> findID: no ID could be determined from name '7777'.")
testthat::expect_warning(selectDWD(7777, res="",var="",per=""),
               "selectDWD: neither station ID nor valid FTP folder is given.")
testthat::expect_warning(selectDWD(id=7777, res="",var="",per=""),
               "selectDWD: in file index 'fileIndex', no filename could be detected with ID 7777.")
testthat::expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "according to file index 'fileIndex', the path '/dummy/dummy/' doesn't exist.")
testthat::expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "according to file index 'fileIndex', there is no file in '/dummy/dummy/' with ID NA.")
testthat::expect_warning(selectDWD(res="dummy", var="", per=""),
               "selectDWD: neither station ID nor valid FTP folder is given.")
testthat::expect_warning(selectDWD(res="daily", var="", per="r"),
               "selectDWD: neither station ID nor valid FTP folder is given.")
testthat::expect_warning(selectDWD(res="daily", var="kl", per=""),
               "according to file index 'fileIndex', there is no file in '/daily/kl/' with ID NA.")
testthat::expect_warning(selectDWD(id="01050", res=c("daily","monthly"), var="kl", per=""), # needs 'per'
               "according to file index 'fileIndex', there is no file in '/daily/kl/' with ID 1050.") 
testthat::expect_warning(selectDWD(id="00386", res="",var="",per="", meta=TRUE),
               "selectDWD: meta is ignored if id is given, but path is not given.")
testthat::expect_warning(selectDWD("Potsdam", res="multi_annual", var="mean_81-10", per=""),
               "selectDWD: multi_annual data is not organized by station ID")
testthat::expect_warning(selectDWD(res="multi_annual", var="mean_81-10", per="r"),
               "selectDWD: multi_annual data is not organized in period folders")

testthat::expect_error(selectDWD(id="Potsdam", res="daily", var="solar"),
             "selectDWD: id may not contain letters: Potsdam")
testthat::expect_error(selectDWD(id="", current=TRUE, res="",var="",per=""),
             "selectDWD: current=TRUE, but no valid paths available.")
})


# checkIndex ----

checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex, fast=fast,
          logfile=paste0(dir_exmpl,"/warnings.txt"), warn=FALSE)


# Index up to date? ----

messaget("++ Testing index up to date?")

# simply try all files for Potsdam (for 1_minute and 10_minutes only 1 each)
if(all_Potsdam_files) 
testthat::test_that("index is up to date - all files can be downloaded and read", {
links <- selectDWD("Potsdam","","","") # does not include multi_annual data!
toexclude <- grep("1_minute", links)
toexclude <- toexclude[-(length(toexclude)-3)]
toexclude <- c(toexclude, grep("10_minutes", links)[-1])
files <- dataDWD(links[-toexclude], dir=dir_data, force=NA, overwrite=TRUE, read=FALSE)
contents <- readDWD(files)
})


messaget("assuming updateIndexes() has been run.")
testthat::test_that("historical files have been updated by DWD", {
# data("fileIndex")
lastyear <- as.numeric(format(Sys.Date(), "%Y"))-1 # the last completed year
outdated <- fileIndex$end==as.Date(paste0(lastyear-1, "-12-31")) & # ends 1 year before lastyear
            fileIndex$per=="historical" & 
            fileIndex$res!="1_minute"
outdated[is.na(outdated)] <- FALSE
sum(outdated)
#View(fileIndex[outdated,])
if(any(outdated)){
rvp <- unique(fileIndex[outdated,1:3])
alloutdated <- sapply(1:nrow(rvp), function(r) 
 {
 fi <- fileIndex$res==rvp[r, "res"] &
  fileIndex$var==rvp[r, "var"] &
  fileIndex$per==rvp[r, "per"]
 all(fi[outdated])
 })
rvp <- apply(rvp, 1, paste, collapse="/")
rvp <- unname(rvp)
if(any(alloutdated)) stop("The DWD has not yet updated any historical files in ",
                          "the following ", sum(alloutdated), " folders:\n", 
                          toString(rvp[alloutdated]))
}})


# Testing examples ----
if(examples)
  {
  checkSuggestedPackage("roxygen2", "runLocalTests with examples=TRUE")
  messaget("++ Testing examples")
  roxygen2::roxygenise()
  oo <- options(rdwdquiet=TRUE)
  berryFunctions::testExamples(logfolder=dir_exmpl, telldocument=FALSE) # version >= 1.18.18
  options(oo)
  }

# Output ----
runtime <- round(difftime(Sys.time(), begintime, units="min"),1)
if(!quiet) message("++ Testing finished!  Total run time: ", runtime, " minutes")
return(invisible(runtime))
}
