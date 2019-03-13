library(testthat)
library(rdwd)

# dataDWD ----------------------------------------------------------------------
message("dataDWD, readDWD")

file <- dataDWD(link, read=FALSE, dir=tempdir(), quiet=TRUE)
test_that("dataDWD works", {
 expect_equal(basename(file), "daily_kl_recent_tageswerte_KL_03987_akt.zip")
})


# readDWD ----------------------------------------------------------------------

clim <- readDWD(file)
supposedcolnames <- c("STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM", "QN_4", 
                      "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", 
                      "UPM", "TXK", "TNK", "TGK", "eor")
test_that("readDWD works", {
 expect_equal(colnames(clim), supposedcolnames)
})


# selectDWD --------------------------------------------------------------------
message("selectDWD")

link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
test_that("selectDWD works", {
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
expect_gte(length(allzip_id[[1]]), 211)
expect_gte(length(allzip_id[[2]]), 7)
expect_gte(length(allzip_folder), 573)
})


# selectDWD warnings -----------------------------------------------------------
message("selectDWD warnings")

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

message("Testing finished!")
