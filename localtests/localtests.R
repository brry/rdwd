library(testthat)
library(rdwd)

link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
test_that("selectDWD works", {
 expect_equal(link, "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/tageswerte_KL_03987_akt.zip")
})


file <- dataDWD(link, read=FALSE, dir=tempdir(), quiet=TRUE)
test_that("dataDWD works", {
 expect_equal(basename(file), "daily_kl_recent_tageswerte_KL_03987_akt.zip")
})


clim <- readDWD(file)
supposedcolnames <- c("STATIONS_ID", "MESS_DATUM", "QN_3", "FX", "FM", "QN_4", 
                      "RSK", "RSKF", "SDK", "SHK_TAG", "NM", "VPM", "PM", "TMK", 
                      "UPM", "TXK", "TNK", "TGK", "eor")
test_that("readDWD works", {
 expect_equal(colnames(clim), supposedcolnames)
})
