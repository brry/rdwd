library(testthat)
library(rdwd)

# reusable data location without version control
# to avoid multiple downloads of the same file
datadir <- paste0(berryFunctions::packagePath(), "/localtests/CreateVignettes/DWDdata")
begintime <- Sys.time()


# dataDWD ----------------------------------------------------------------------

message("++ Testing dataDWD, readDWD")

test_that("dataDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=datadir, quiet=TRUE)
expect_equal(basename(file), "daily_kl_recent_tageswerte_KL_03987_akt.zip")
links <- selectDWD(id=c(5302,5711,6295),res="daily",var="more_precip",per="h")
expect_error(dataDWD(links, dir=datadir), "file must be a vector, not a list")
})


# readDWD ----------------------------------------------------------------------

# . readDWD.data ----
test_that("readDWD.data works for regular data", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
file <- dataDWD(link, read=FALSE, dir=datadir, quiet=TRUE)
clim <- readDWD(file)
supposedcolnames <- c("STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM", "QN_4", 
                      "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", 
                      "UPM", "TXK", "TNK", "TGK", "eor")
expect_equal(colnames(clim), supposedcolnames)
climf <- readDWD(file, fread=TRUE)
expect_equal(clim, climf)
})

# . readDWD.meta ----
test_that("readDWD.meta works for Beschreibung data", {
link <- selectDWD(res="daily", var="kl", per="r", meta=TRUE)
if(length(link)!=1) stop("length of link should be 1, but is ", length(link), 
                         ":\n", berryFunctions::truncMessage(link,prefix="",sep="\n"))
meta <- dataDWD(link, dir=datadir)
cnm <- colnames(meta)
if(length(cnm)!=8) stop("number of columns should be 8, but is ", length(cnm),
                        ":\n", toString(cnm))
})


# . readDWD.binary ----
test_that("readDWD.binary works for radolan grid data", {
gridbase <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany"
radfile <- "/daily/radolan/historical/2017/SF201712.tar.gz"
# 204 MB, takes a minute to download:
localfile <- dataDWD(file=paste0(gridbase, radfile), base=gridbase, 
                     dir=datadir, read=FALSE)
rad <- readDWD(localfile, selection=1:10) # no need to read all 24*31=744 files
if(length(rad)!=10) stop("length(rad) should be 10, but is ", length(rad))
# ToDo: make sense of the values, read them correctly!
warning("readDWD.binary does not yet read the binary files correctly.")
raster::plot(raster::raster(matrix(rad[[1]], ncol=900, byrow=TRUE)))
})

message("++ Testing readDWD raster + multia methods")

# . readDWD.raster ----
test_that("readDWD.raster works for raster data", {
rasterbase <- "ftp://ftp-cdc.dwd.de/pub/CDC/grids_germany/seasonal/air_temperature_mean"
ftp.files <- indexFTP("/16_DJF", base=rasterbase, dir=tempdir())
localfiles <- dataDWD(paste0(rasterbase,"/",ftp.files[1:2]), base=rasterbase,
                      dir=datadir, read=FALSE)
rf <- readDWD(localfiles[1])
rf <- readDWD(localfiles[1]) # needs to be able to run a second time
raster::plot(rf) 
expect_equal(raster::cellStats(rf, range), c(-8.2,4.4))
})


# . readDWD.multia ----
test_that("readDWD.multia works for multi_annual data", {
durl <- selectDWD(res="multi_annual", var="mean_81-10", per="")[9] # Temperature aggregates
murl <- selectDWD(res="multi_annual", var="mean_81-10", per="", meta=TRUE)[9]
ma_temp <- dataDWD(durl, dir=datadir)
ma_meta <- dataDWD(murl, dir=datadir)
})


# findID -----------------------------------------------------------------------

test_that("findID warns as wanted", {
expect_warning(findID("this_is_not_a_city"),
               "findID: no ID could be determined from name 'this_is_not_a_city'.")
expect_warning(findID(c("Wuppertal","this_is_not_a_city") ),
               "findID: no ID could be determined from name 'this_is_not_a_city'.")
expect_warning(findID(7777),
               "findID: no ID could be determined from name '7777'.")
expect_warning(findID("01050"),
               "findID: no ID could be determined from name '01050'.")
expect_equal(findID(), "")
})


# selectDWD --------------------------------------------------------------------

message("++ Testing selectDWD")

test_that("selectDWD works", {
link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
expect_equal(link, paste0(dwdbase,"/daily/kl/recent/tageswerte_KL_03987_akt.zip"))
expect_equal(selectDWD("Potsdam", res="daily", var="solar"),
             paste0(dwdbase,"/daily/solar/tageswerte_ST_03987_row.zip"))
})

test_that("selectDWD id input can be numeric or character", {
expect_equal(selectDWD(id="00386", res="daily", var="kl", per="historical"),
             selectDWD(id=386,     res="daily", var="kl", per="historical"))
})

test_that("selectDWD can choose Beschreibung meta files", {
expect_equal(selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE),
  paste0(dwdbase, "/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"))
})


test_that("selectDWD properly vectorizes", {
expect_type(selectDWD(id="01050", res="daily", var="kl", per=c("r","h")), "list")
expect_type(selectDWD(id="01050", res="daily", var="kl", per="rh"), "character")
# all zip files in all paths matching id:
allzip_id <- selectDWD(id=c(1050, 386), res="",var="",per="")
# all zip files in a given path (if ID is empty):
allzip_folder <- selectDWD(id="", res="daily", var="kl", per="recent") 
expect_equal(length(allzip_id), 2)
expect_gte(length(allzip_id[[1]]), 200)
expect_gte(length(allzip_id[[2]]), 7)
expect_gte(length(allzip_folder), 573)
})


# selectDWD warnings -----------------------------------------------------------

message("++ Testing selectDWD warnings")

test_that("selectDWD warns as intended", {
expect_warning(selectDWD(res="",var="",per=""), 
               "selectDWD: neither station ID nor valid FTP folder is given.")
expect_warning(selectDWD(7777, res="",var="",per=""),
               "selectDWD -> findID: no ID could be determined from name '7777'.")
expect_warning(selectDWD(7777, res="",var="",per=""),
               "selectDWD: neither station ID nor valid FTP folder is given.")
expect_warning(selectDWD(id=7777, res="",var="",per=""),
               "selectDWD: in file index 'fileIndex', no filename could be detected with ID 7777.")
expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "according to file index 'fileIndex', the path '/dummy/dummy/' doesn't exist.")
expect_warning(selectDWD(id="", res="dummy", var="dummy", per=""),
               "according to file index 'fileIndex', there is no file in '/dummy/dummy/' with ID NA.")
expect_warning(selectDWD(res="dummy", var="", per=""),
               "selectDWD: neither station ID nor valid FTP folder is given.")
expect_warning(selectDWD(res="daily", var="", per="r"),
               "selectDWD: neither station ID nor valid FTP folder is given.")
expect_warning(selectDWD(res="daily", var="kl", per=""),
               "according to file index 'fileIndex', there is no file in '/daily/kl/' with ID NA.")
expect_warning(selectDWD(id="01050", res=c("daily","monthly"), var="kl", per=""), # needs 'per'
               "according to file index 'fileIndex', there is no file in '/daily/kl/' with ID 1050.") 
expect_warning(selectDWD(id="00386", res="",var="",per="", meta=TRUE),
               "selectDWD: meta is ignored if id is given, but path is not given.")

expect_error(selectDWD(id="Potsdam", res="daily", var="solar"),
             "selectDWD: id may not contain letters: Potsdam")
expect_error(selectDWD(id="", current=TRUE, res="",var="",per=""),
             "selectDWD: current=TRUE, but no valid paths available.")
})


# Index up to date? ------------------------------------------------------------

message("++ Testing index up to date?")

# simply try all files for Potsdam (for 1_minute and 10_minutes only 1 each)
test_that("index is up to date", {
links <- selectDWD("Potsdam","","","")
toexclude <- grep("1_minute", links)
toexclude <- toexclude[-(length(toexclude)-3)]
toexclude <- c(toexclude, grep("10_minutes", links)[-1])
files <- dataDWD(links[-toexclude], dir=datadir, force=NA, read=FALSE)
contents <- readDWD(files)
})


test_that("historical files have been updated by DWD", {
data("fileIndex")
lastyear <- as.numeric(format(Sys.Date(), "%Y"))-1 # the last completed year
outdated <- fileIndex$end==paste0(lastyear-1, "1231") & # ends 1 year before lastyear
            fileIndex$per=="historical" & 
            fileIndex$res!="1_minute"
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


# Testing examples -------------------------------------------------------------

devtools::document()
berryFunctions::testExamples(logfolder="localtests/ExampleTests")

message("++ Testing finished!  Total run time: ", 
        round(difftime(Sys.time(), begintime, units="min"),1), " minutes")

