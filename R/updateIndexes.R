#' @title update rdwd indexes
#' @description This is meant to be called with getwd at the
#'  rdwd package source code directory to update the indexes with one single call.\cr
#'  To use custom or current indexes, see <https://bookdown.org/brry/rdwd/fileindex.html>
#' @return [checkIndex()] results
#' @importFrom berryFunctions sortDF
#' @importFrom tools resaveRdaFiles
#' @importFrom utils read.table write.table
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2019
#' @seealso [createIndex()]
#' @keywords data file internal
#' @examples
#' # number of files at dwdbase
#' #  25'757 (2017-03-14)
#' # 218'593 (2018-03-25)
#' # 228'830 (2018-11-26)
#' # 240'737 (2019-02-19)
#' # 242'584 (2019-03-11)
#' # 266'860 (2019-05-15)
#' # 254'446 (2019-05-30)
#' # 255'252 (2019-07-31)
#' # 254'925 (2019-09-17)
#' # 254'943 (2019-10-26)
#' # 266'841 (2020-03-16)
#' # 265'712 (2020-04-10)
#' # 265'712 (2020-04-24)
#' # 266'106 (2020-06-01)
#' # 266'216 (2020-07-06)
#' # 266'216 (2020-07-28)
#' # 267'175 (2020-09-21)
#' # 269'561 (2020-12-03)
#' # 286'306 (2021-04-02)
#' # 286'189 (2021-04-08)
#' # 285'246 (2021-04-23)
#' # 285'972 (2021-06-02)
#' # 321'477 (2022-04-07)
#' # 477'236 (2022-04-28) # 5_minutes files added
#' # 482'907 (2022-04-29)
#' # 497'190 (2022-05-13)
#' # 576'768 (2023-04-06) # lots of duplicate historical files in transition period
#' # 577'160 (2023-04-12)
#' # 571'667 (2023-04-14)
#' # 572'583 (2023-05-10)
#' # 574'397 (2023-06-16)
#' # 573'963 (2023-09-16)
#' # 588'527 (2024-04-09)
#' # 586'626 (2024-05-14)
#' # 609'082 (2024-08-12)
#' # 596'417 (2025-01-23)
#' # 630'152 (2025-03-10)
#' # 633'334 (2025-05-20)
#' # 632'723 (2025-06-03)
#' # 634'195 (2025-06-06)
#' # 634'693 (2025-06-21)
#' 
#' # gridbase
#' #  49'247 (2019-05-26)
#' #  49'402 (2019-05-30)
#' #  54'314 (2019-07-31)
#' #  56'759 (2019-09-17)
#' #  58'656 (2019-10-26)
#' #  30'320 (2020-03-16)
#' #  31'787 (2020-04-10)
#' #  32'478 (2020-04-24)
#' #  34'203 (2020-06-01)
#' #  35'953 (2020-07-06)
#' #  37'038 (2020-07-28)
#' #  39'791 (2020-09-21)
#' #  43'435 (2020-12-03)
#' #  31'698 (2021-04-02)
#' #  32'015 (2021-04-08)
#' #  32'736 (2021-04-23)
#' #  34'708 (2021-06-02)
#' #  34'854 (2022-04-07)
#' #  35'874 (2022-04-28)
#' #  35'937 (2022-04-29)
#' #  36'630 (2022-05-13)
#' #  36'059 (2023-04-06)
#' #  36'356 (2023-04-12)
#' #  36'454 (2023-04-14)
#' #  37'763 (2023-05-10)
#' #  39'670 (2023-06-16)
#' #  54'828 (2023-09-16)
#' #  97'923 (2024-04-09)
#' #  64'713 (2024-05-14)
#' #  82'163 (2024-08-12)
#' # 135'436 (2025-01-23)
#' #  83'823 (2025-03-10)
#' #  98'100 (2025-05-20)
#' # 101'298 (2025-06-03)
#' # 101'842 (2025-06-06)
#' # 105'099 (2025-06-21)
#' 
#' @param dwdlocal Read "DWDdata/INDEX_of_DWD_.txt" instead of calling
#'                 [indexFTP()]? DEFAULT: FALSE
#' @param grdlocal Read "DWDdata/INDEX_of_DWD_grids.txt" instead of calling
#'                 [indexFTP()]? DEFAULT: FALSE
#' @param metaforce [dataDWD()] `force` argument for BESCHREIBUNG files.
#'                 DEFAULT: NA (re-download if older than 24 hours)
#' 
updateIndexes <- function(
dwdlocal=FALSE,
grdlocal=FALSE,
metaforce=NA
)
{
# get indexes ----

if(!grepl("rdwd$", getwd())) stop("getwd must be in package root folder.")
begintime <- Sys.time()
messaget <- function(...) message(format(Sys.time(), "%T - "), ...)
# get filenames on FTP server:
if(dwdlocal) dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt") else
 {
 messaget("Indexing FTP Server at dwdbase (expect 0,2,15,47,40s) ...")
 dwdfiles <- indexFTP(sleep=0, filename="", dir="DWDdata", overwrite=TRUE)
 }
if(grdlocal) grdfiles <- readLines("DWDdata/INDEX_of_DWD_grids.txt") else
 {
 messaget("Indexing FTP Server at gridbase (expect 0,2,22,64s,2m,21,57s) ...")
 grdfiles <- indexFTP(filename="grids", base=gridbase, dir="DWDdata", overwrite=TRUE)
 }

messaget("Calling createIndex...")
index <- createIndex(paths=dwdfiles, meta=TRUE, force=metaforce, dir="DWDdata", overwrite=TRUE, checkwarn=FALSE)

# save indexes ----

messaget("Saving index rda files into package...")
fileIndex <- index[[1]]
metaIndex <- index[[2]]
 geoIndex <- index[[3]]
gridIndex <- grdfiles
# to enable R versions <3.5.0 (2018-04, only one year old at time of writing)
# version=2 see https://github.com/r-lib/devtools/issues/1912
save(fileIndex, file="data/fileIndex.rda", version=2)
save(metaIndex, file="data/metaIndex.rda", version=2)
save( geoIndex, file="data/geoIndex.rda" , version=2)
save(gridIndex, file="data/gridIndex.rda", version=2)
#
messaget("Compressing index rda files:")
tools::resaveRdaFiles("data/fileIndex.rda", version=2) #devtools::use_data(fileIndex, internal=TRUE)
cat("1")
tools::resaveRdaFiles("data/metaIndex.rda", version=2)
cat("2")
tools::resaveRdaFiles("data/geoIndex.rda" , version=2)
cat("3")
tools::resaveRdaFiles("data/gridIndex.rda", version=2)
cat("4\n")
message("Checking rda file contents...")
# check writing and reading of the files:
fileIndex2 <- read.table("DWDdata/fileIndex.txt", sep="\t", header=TRUE, colClasses="character")
stopifnot(all(fileIndex==fileIndex2, na.rm=TRUE)) # NAs in ID for subdaily/multi_annual
metaIndex2 <- read.table("DWDdata/metaIndex.txt", sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all(metaIndex==metaIndex2))
 geoIndex2 <- read.table("DWDdata/geoIndex.txt",  sep="\t", header=TRUE, as.is=TRUE)
stopifnot(all( geoIndex== geoIndex2))


# read and save subdaily format description ----

messaget("Reading subdaily/standard_format description html file...")
checkSuggestedPackage("XML", "updateIndexes")
format_url <- paste0(dwdbase,"/subdaily/standard_format/formate_kl.html")
format_html <- readLines(format_url, encoding="UTF-8")
message("Saving and compressing formatIndex...")
format_html <- gsub("&nbsp;", "", format_html)
format_html <- gsub("&#176;", "degree", format_html)
format_html <- format_html[!grepl("Formatbeschreibung", format_html)]
formatIndex <- XML::readHTMLTable(doc=format_html, header=TRUE, stringsAsFactors=FALSE)[[1]]
formatIndex$dividebyten <- grepl("0.1", formatIndex$Einheit)
formatIndex$Einheit <- gsub("0.1 ", "", formatIndex$Einheit)
save(formatIndex, file="data/formatIndex.rda", version=2)
tools::resaveRdaFiles( "data/formatIndex.rda", version=2)


# readVars parameter abbreviations ----

messaget("Checking readVars parameter abbreviations...")
urls <- selectDWD("Potsdam","","","", quiet=TRUE, mindex=metaIndex, findex=fileIndex)
urls <- urls[!grepl("meta_data", urls)]
urls <- urls[!grepl("minute", urls)]
files <- dataDWD(urls, dir=locdir(), read=FALSE)
rv <- readVars(files, quiet=TRUE) # quiet, message through nkurzmissing
#str(rv, max.level=1)
k <- unlist(lapply(rv, function(x)x$Kurz))
nkurzmissing <- sum(is.na(k))
#
if(nkurzmissing!=0)
{
message(nkurzmissing, "/", length(k), " DWD abbreviations have no Kurz entry.")
rv_df <- do.call(rbind, rv)
rv_df$Quelle <- rep(substr(urls, 76, 1e3), sapply(rv, nrow))
rv_df <- berryFunctions::sortDF(rv_df, "Par", decreasing=FALSE)
rv_df <- berryFunctions::sortDF(rv_df, "Kurz", decreasing=FALSE)
colnames(rv_df)[1] <- "Parameter"
write.table(rv_df, "misc/params.txt", sep="\t", quote=FALSE, row.names=FALSE)
message("- Copy content of 'misc/params.txt' to 'params_input' at
https://docs.google.com/spreadsheets/d/1qXQ1bSLW5TJnJgpUXIID3mVNYS6YZaHbsoe22LmBIAk/edit#gid=100501290
- Manually add 'Kurz' entries.
- Copy sheet 'params_output' to dwdparams in R/readVars.R")
}
#
# check for duplicates:
dupli <- sapply(rv, function(x) sum(duplicated(x[,"Kurz"]))>0)
if(any(dupli)) message("Duplicate Kurz entries in:\n-", 
                       truncMessage(names(dupli)[dupli], prefix="", sep="\n- ", ntrunc=8))



# Final messages ----

message(" - To view the checkIndex results, use   cat(.Last.value)")

a <- paste0("#' # ", formatC(length(dwdfiles), big.mark="'"), " (",Sys.Date(), ")")
b <- paste0("#' #  ",formatC(length(grdfiles), big.mark="'"), " (",Sys.Date(), ")")
message("- Manually add these file numbers to the examples in R/updateIndexes.R:\n", a, "\n", b)

dt <- Sys.time()-begintime
message("Done! updateIndex execution took ", round(dt,2), " ", attr(dt, "units"), ".")
return(invisible(index$checks))
}
