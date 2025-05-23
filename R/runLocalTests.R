#' @title run local tests of rdwd
#' @description Run `rdwd` tests on local machine. Due to time-intensive
#' data downloads, these tests are not run automatically on CRAN.
#' @return Time taken to run tests in minutes
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr-Oct 2019
#' @seealso [locdir()]
#' @keywords debugging
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par title
#' @importFrom utils tail
#' @export
#' 
#' @param dir_data  Reusable data location. Preferably not under version control.
#'                  DEFAULT: [locdir()]
#' @param dir_exmpl Reusable example location. DEFAULT: local directory
#' @param start     Number to start tests at, helpful for partially successful runs.
#'                  DEFAULT: 1
#' @param quiet Suppress progress messages? DEFAULT: FALSE through [rdwdquiet()]
#' 
runLocalTests <- function(
dir_data=locdir(),
dir_exmpl=berryFunctions::packagePath(file="misc/ExampleTests"),
start=1,
quiet=rdwdquiet()
)
{
# pre-checks ----
checkSuggestedPackage("testthat", "runLocalTests")
if(!grepl("rdwd$", getwd())) stop("getwd must be in package root folder.")
begintime <- Sys.time()
messaget <- function(x) if(!quiet) message(x, " (",
          round(difftime(Sys.time(), begintime, units="s")), " secs so far)")

# clear warnings logfile:
cat("", file=paste0(dir_exmpl,"/warnings.txt"))
cat("", file=paste0(dir_exmpl,"/errors.txt"))


# readDWD.data ----

messaget("++ Testing dataDWD + readDWD.data")

if(start<=1) testthat::test_that("1. dataDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=dir_data, quiet=TRUE)
testthat::expect_equal(basename(file), "daily_kl_recent_tageswerte_KL_03987_akt.zip")
links <- selectDWD(id=c(5302,5711,3987),res="daily",var="more_precip",per="h")
testthat::expect_warning(dataDWD("multi/mean/Temp.txt", quiet=TRUE),
               "dataDWD needs urls starting with 'ftp://'.")
f <- paste0(dwdbase, "/daily/kl/historical/tageswerte_KL_03987_18930101_20181231_hist.zip")
testthat::expect_warning(dataDWD(f, quiet=TRUE), "If files have been renamed on the DWD server")
testthat::expect_warning(dataDWD(c("multi/mean/Temp.txt", f), quiet=TRUE),
                         "urls starting with .* renamed on the DWD server")
})

if(start<=2) testthat::test_that("2. readDWD.data works for regular data", {
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

if(start<=3) testthat::test_that("3. readDWD.data works for 10 minute data", {
link <- selectDWD("Kiel-Holtenau", res="10_minutes", var="air_temperature", per="recent")
file <- dataDWD(link, read=FALSE, dir=dir_data)
air_temperature <- readDWD(file, varnames=TRUE)
time_diff <- as.numeric(diff(air_temperature$MESS_DATUM[1:10]))
testthat::expect_equal(time_diff, rep(10,9))
})

if(start<=4) testthat::test_that("4. readDWD.data works with hr", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="hr")
kl <- dataDWD(link, hr=4)
testthat::expect_s3_class(kl, "data.frame") # instead of the usual list
# vector of stations:
link <- selectDWD(c("Potsdam","Celle"), res="daily", var="kl", per="hr")
testthat::expect_error(dataDWD(link, hr=4), "hr=4, but 2 ids are given: 850, 3987")
})

if(start<=5) testthat::test_that("5. readDWD messages subfunctions correctly", {
link <- c(selectDWD("Potsdam", res="daily", var="kl", per="hr"),
          selectDWD("", "daily", "kl", " ", meta=TRUE),
          selectDWD("", "daily", "kl", "h", meta=TRUE))
link <- link[!grepl("mn4_Beschreibung",link)] # mn4 file with only 2 Berlin stations, Apr 2022
file <- dataDWD(link, read=FALSE, dir=dir_data, progbar=FALSE)
testthat::expect_message(readDWD(file, progbar=FALSE), 
  "Reading 5 files with readDWD.data (2) / readDWD.meta (1) / readDWD.pdf (2) and fread=TRUE ...", fixed=TRUE)
})


# 3 secs so far
# readRadarFile ----
messaget("++ Testing readRadarFile")

if(!file.exists(dir_exmpl)) dir.create(dir_exmpl)

if(start<=6) testthat::test_that("6. readRadarFile works", {
trr <- function(file, ext="radolan", readdwd=FALSE) # trr: test reading radar data
  {
  main <- deparse(substitute(file))
  file2 <- berryFunctions::packagePath(file=paste0("misc/",file))
  rrf <- if(readdwd) readDWD(file2, toraster=FALSE) else dwdradar::readRadarFile(file2)
  rrr <- terra::rast(rrf$dat)
  rrp <- projectRasterDWD(rrr, extent=ext)
  terra::plot(rrr, main="\nOriginal")
  terra::plot(rrp, main="\nProjected")
  addBorders()
  title(main=main, outer=TRUE, line=-1.1)
  rngr <- range(terra::minmax(rrr))
  rngp <- range(terra::minmax(rrp))
  return(list(file=file2, rrp=rrp, meta=rrf$meta, range_orig=rngr, range_proj=rngp))
  }
pdf(paste0(dir_exmpl,"/Radartests.pdf"), width=10, height=7)
par(mfrow=c(1,2), mar=c(2,2,3,3), mgp=c(2,0.7,0))
w1 <- trr("raa01-rw2017.002_10000-1712310850-dwd---bin_hourRadReproc", ext="rw")
w2 <- trr("raa01-rw_10000-1907311350-dwd---bin_hourRadRecentBin.gz", readdwd=TRUE)
rw <- trr("raa01-rw_10000-1907010950-dwd---bin_weatherRadolan")
sf <- trr("raa01-sf_10000-1605010450-dwd---bin_dailyRadHist")
rx <- trr("raa01-rx_10000-1605290600-dwd---bin_Braunsbach")
rx1 <- terra::rast(dwdradar::readRadarFile(rx$file)$dat)
rx2 <- projectRasterDWD(rx1, targetproj=NULL)
terra::plot(rx2, main="\nProjected without latlon")
terra::plot(rx$rrp, zlim=rx$range_orig, main="\nProjected, with custom zlim")
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
  }                                             # before terra (June 2023)
rangecheck(w1, c( 0.0,  6.2 ), c( 0.00,  5.67)) # was  0.00,  5.87 
rangecheck(w2, c( 0.0,  7.26), c( 0.00,  7.03)) # was -0.02,  7.09  and 72.6 instead of 7.26 (Apr 2022)
rangecheck(rw, c( 0.0, 30.7 ), c( 0.00, 27.21)) # was -0.45, 30.45 
rangecheck(sf, c( 0.0, 39.2 ), c( 0.00, 38.13)) # was -0.03, 38.20
rangecheck(rx, c(31.5, 95.0 ), c(31.50, 95.00)) # was 18.30, 97.17
testthat::expect_equal("Radar tests passed", "Radar tests passed")
})


# 12 secs so far
# findID ----
messaget("++ Testing findID + selectDWD")

if(start<=7) testthat::test_that("7. findID warns as wanted", {
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

if(start<=8) testthat::test_that("8. indexFTP warns and works as intended", {
base <- "https://opendata.dwd.de/weather/radar/radolan/rw/"
testthat::expect_error(indexFTP(base, folder="", dir=tempdir(), quiet=TRUE),
                         "base should start with ftp://")
base <- "ftp://opendata.dwd.de/weather/radar/radolan/rw"
rw <- indexFTP(base, folder="", dir=tempdir(), quiet=TRUE, exclude.latest.bin=FALSE)
testthat::expect_true("/raa01-rw_10000-latest-dwd---bin.bz2" %in% tail(rw,2))
# File ending .bz2 added 2023-04-12. Also updated in website use case recent radar.
})

if(start<=8) testthat::test_that("8b. createIndex works", {
checkSuggestedPackage("gsheet", "runLocalTests")
iex <- gsheet::gsheet2tbl("1qXQ1bSLW5TJnJgpUXIID3mVNYS6YZaHbsoe22LmBIAk")
fex <- createIndex(iex$link, dir=tempdir())
fex[fex==""] <- NA # for comparison in test
# fex$path <- NULL ; clipr::write_clip(fex)
testthat::expect_equal(iex$link  , fex$path  )
testthat::expect_equal(iex$res   , fex$res   )
testthat::expect_equal(iex$var   , fex$var   )
testthat::expect_equal(iex$per   , fex$per   )
testthat::expect_equal(iex$id    , fex$id    )
testthat::expect_equal(iex$start , fex$start )
testthat::expect_equal(iex$end   , fex$end   )
testthat::expect_equal(iex$ismeta, fex$ismeta)
})

# selectDWD ----

if(start<=9) testthat::test_that("9. selectDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
testthat::expect_equal(link, paste0(dwdbase,"/daily/kl/recent/tageswerte_KL_03987_akt.zip"))
testthat::expect_equal(selectDWD("Potsdam", res="daily", var="solar"),
             paste0(dwdbase,"/daily/solar/tageswerte_ST_03987_row.zip"))
})

if(start<=10) testthat::test_that("10. selectDWD id input can be numeric or character", {
testthat::expect_equal(selectDWD(id="00386", res="daily", var="kl", per="historical"),
             selectDWD(id=386,     res="daily", var="kl", per="historical"))
})

if(start<=11) testthat::test_that("11. selectDWD can choose Beschreibung meta files", {
link <- selectDWD(res="daily", var="kl", per="h", meta=TRUE)
link <- link[!grepl("mn4_Beschreibung",link)]
link <- grep(".txt$", link, value=TRUE)
testthat::expect_equal(link,
  paste0(dwdbase, "/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"))
})


if(start<=12) testthat::test_that("12. selectDWD properly vectorizes", {
testthat::expect_type(selectDWD(id="01050", res="daily", var="kl", per=c("r","h")), "character")
testthat::expect_type(selectDWD(id="01050", res="daily", var="kl", per="rh"), "character")
# all zip files in all paths matching id:
allzip_id <- selectDWD(id=c(1050, 386), res="",var="",per="", quiet=TRUE)
# all zip files in a given path (if ID is empty):
allzip_folder <- selectDWD(id="", res="daily", var="kl", per="recent")
testthat::expect_gte(length(allzip_id), 277)
testthat::expect_gte(length(allzip_folder), 571)
# changed 573 to 571 2023-04-12. Should be OK either way.
ids <- c(3761,3761, 3603)
exT <- selectDWD(id=ids, res="daily", var="kl", per=c("h","r","r")) # 4
exF <- selectDWD(id=ids, res="daily", var="kl", per=c("h","r","r"), expand=FALSE) # 3
testthat::expect_equal(length(exT), 4)
testthat::expect_equal(length(exF), 3)
})

# 14 secs so far
# selectDWD warnings ----
messaget("++ Testing selectDWD warnings")

oop <- options(rdwdquiet=FALSE)
if(start<=13) testthat::test_that("13. selectDWD warns as intended", {
testthat::expect_error(selectDWD(res="",var="",per=""),
               "selectDWD: One \\(or both\\) of 'id' and 'res/var/per' must be given.")
testthat::expect_error(
testthat::expect_warning(selectDWD(7777, res="",var="",per="", failempty=FALSE),
               "-> findID: no ID could be determined from name '7777'."),
          "of 'id' and 'res/var/per' must be given.")
testthat::expect_error(selectDWD("dummy", res="daily",var="kl",per="r"),
                       "-> findID: no ID could be determined from name 'dummy'.")
testthat::expect_warning(
      dd <- selectDWD("dummy", res="daily",var="kl",per="r", failempty=FALSE),
      "-> findID: no ID could be determined from name 'dummy'.")
testthat::expect_gte(length(dd), 400) # 571 in 2023-04
testthat::expect_warning(selectDWD(id=7777, res="",var="",per=""),
               "selectDWD: No entries in file index 'fileIndex' match your query")
testthat::expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "selectDWD: No entries in file index 'fileIndex' match your query")
testthat::expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "selectDWD: No entries in file index 'fileIndex' match your query")
testthat::expect_warning(selectDWD(res="dummy", var="", per=""),
               "selectDWD: No entries in file index 'fileIndex' match your query")
testthat::expect_gte(length(selectDWD(res="daily", var="",   per="r")), 3404) # 3414 to 3404 2023-04
testthat::expect_gte(length(selectDWD(res="daily", var="kl", per="" )), 1863)
testthat::expect_length(selectDWD(id="01050", res=c("daily","monthly"), var="kl", per=""), 4)
testthat::expect_warning(selectDWD(id="00386", res="",var="",per="", meta=TRUE),
               "selectDWD: No entries in file index 'fileIndex' match your query")
testthat::expect_warning(selectDWD("Potsdam", res="multi_annual", var="mean_81-10", per=""),
               "selectDWD: Setting id and var to '' for multi_annual data. Use per to choose period.")
testthat::expect_length(selectDWD("", res="multi_annual", per="mean_81-10"), 8)
testthat::expect_warning(selectDWD("Potsdam", "annual", "kl", "r", dude=55),
                "selectDWD: unused arguments: dude")
testthat::expect_error(selectDWD(id="Potsdam", res="daily", var="solar"),
             "selectDWD: id may not contain letters: Potsdam")
testthat::expect_error(selectDWD(id="", current=TRUE, res="",var="",per=""),
             "of 'id' and 'res/var/per' must be given.")
testthat::expect_error(selectDWD("Potsdam", res="", var="kl", per="r", expand=FALSE), 
             "selectDWD: With expand=FALSE, 'res' may not be NA or '' \\(empty\\).")
})
options(oop)
rm(oop)

# 14 secs so far
messaget("++ Running checkindex, expect 1 minute")
# checkIndex ----
if(start<=14) testthat::test_that("14. checkIndex runs", {
checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex,
          logfile=paste0(dir_exmpl,"/warnings.txt"), warn=FALSE)
testthat::expect_equal(1,1) # silence message about skipping empty test
})

# 70 secs so far
# Index up to date? ----
messaget("++ Testing index up to date? Expect 20 seconds, abort if needed.")
messaget("assuming updateIndexes() has been run.")

if(start<=15) testthat::test_that("15. index is up to date - all files can be downloaded and read", {
testthat::expect_message(checkUpdates(), "The historical index files are fully present on the DWD server")
# keeping the old test for now:
# simply try all files for Potsdam (for 1/5/10_minutes only 1 each)
links <- selectDWD("Potsdam","","","", quiet=TRUE) # does not include multi_annual data!
toexclude <- grep("1_minute", links)
toexclude <- toexclude[-(length(toexclude)-3)]
toexclude <- c(toexclude, grep("10_minutes", links)[-1])
toexclude <- c(toexclude, grep("5_minutes", links)[-1])
links <- links[-toexclude]
links <- links[!grepl("meta_data/Meta_Daten", links)]
files <- dataDWD(links, dir=dir_data, force=NA, overwrite=TRUE, read=FALSE, progbar=TRUE)
contents <- readDWD(sample(files), progbar=TRUE)
testthat::expect_length(contents, length(files))
})

# 91 secs so far
if(start<=16) testthat::test_that("16. historical files have been updated by DWD", {
# moved to checkUpdates - would yield a warning if applicable.
testthat::expect_equal(1,1) # silence message about skipping empty test
})


# devtools::check ----
messaget("++ Running devtools::check")

if(start<=17) testthat::test_that("17. devtools::check runs", {
checkSuggestedPackage("devtools", "runLocalTests")
dd <- devtools::check(quiet=quiet)
print(dd)
testthat::expect_equal(1,1) # silence message about skipping empty test
})

# 18. Testing examples ----
messaget("++ Testing examples")

if(start<=18) {
checkSuggestedPackage("roxygen2", "runLocalTests with examples=TRUE")
if(start>=18) roxygen2::roxygenise()
oo <- options(rdwdquiet=TRUE)
berryFunctions::testExamples(logfolder=dir_exmpl, telldocument=FALSE) # version >= 1.18.18
options(oo)
# remove false positives in warnings.txt
logfile <- paste0(dir_exmpl,"/warnings.txt")
log <- readLines(logfile)
log <- paste0(log, collapse="\n")
rem <- "\nList of 7\n \\$ Metadaten_Fehldaten_05856_.*obs. of  4 variables:"
log <- sub(rem, "", log)
rem <- "Warning: fileType failed for the following file: 'random_stuff.odt'\n"
log <- sub(rem, "", log, fixed=TRUE)
rem <- "\nrdwd station id 2849 with 6 files.
Name: Langenburg-Baechlingen, State: Baden-Wuerttemberg
For up-to-date info, see https://bookdown.org/brry/rdwd/fileindex.html#metaindex
Additionally, there are 6 non-public files. Display all with  metaInfo(2849,FALSE)
To request those datasets, please contact cdc.daten@dwd.de or klima.vertrieb@dwd.de
      res                    var        per hasfile       from         to     lat   long ele
1  annual        climate_indices historical    TRUE 1991-01-01 2007-12-31 49.2445 9.8499 300
2  annual            more_precip historical    TRUE 1990-10-01 2008-06-30 49.2445 9.8499 300
3   daily            more_precip historical    TRUE 1990-10-01 2008-06-30 49.2445 9.8499 300
4   daily more_weather_phenomena historical    TRUE 1990-10-01 2008-06-30 49.2445 9.8499 300
5 monthly        climate_indices historical    TRUE 1990-10-01 2008-06-30 49.2445 9.8499 300
6 monthly            more_precip historical    TRUE 1990-10-01 2008-06-30 49.2445 9.8499 300
"
log <- sub(rem, "", log, fixed=TRUE)
rem <- "\nNote in is.error: Error in plotRadar(ncp1, layer = 1:4, project = FALSE) : 
  3 layers selected that do not exist.\n"
log <- sub(rem, "", log, fixed=TRUE)
rem <- "ToDO: replace this with the terra output"

log <- sub(rem, "", log, fixed=TRUE)
rem <- "---------------\n.*/Dropbox/Rpack/rdwd/man/.{10,50}\n\n\n"
log <- gsub(rem, "", log)
rem <- "---------------\n.*/Dropbox/Rpack/rdwd/man/updateRdwd.Rd -- .{19}"
log <- sub(rem, "", log)
cat(log, file=logfile)
testthat::expect_equal(1,1) # silence message about skipping empty test
}


# Output ----
runtime <- round(difftime(Sys.time(), begintime, units="min"),1)
if(!quiet) message("++ Testing finished!  Total run time: ", runtime, " minutes")
return(invisible(runtime))
}
