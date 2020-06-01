#' @title update rdwd indexes
#' @description This is meant to be called with getwd at the 
#'  rdwd package source code directory to update the indexes with one single call.
#' @return \code{\link{checkIndex}} results
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2019
#' @seealso \code{\link{createIndex}}, \code{graphics::\link[graphics]{plot}}
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
#' 
#' @param dwdlocal Read "DWDdata/INDEX_of_DWD_.txt" instead of calling
#'                 \code{\link{indexFTP}}? DEFAULT: FALSE
#' @param grdlocal Read "DWDdata/INDEX_of_DWD_grids.txt" instead of calling
#'                 \code{\link{indexFTP}}? DEFAULT: FALSE
#' @param metaforce \code{\link{dataDWD} force} argument for BESCHREIBUNG files. 
#'                 DEFAULT: NA (re-download if older than 24 hours)
#'
updateIndexes <- function(
dwdlocal=FALSE,
grdlocal=FALSE,
metaforce=NA
)
{
# get indexes ----

if(!grepl("rdwd$", getwd())) stop("getwd must be in package root folder.", immediate.=TRUE)
begintime <- Sys.time()
messaget <- function(...) message(format(Sys.time(), "%T - "), ...)
# get filenames on FTP server:
if(dwdlocal) dwdfiles <- readLines("DWDdata/INDEX_of_DWD_.txt") else
 {
 messaget("Indexing FTP Server at dwdbase...")
 dwdfiles <- indexFTP(sleep=0, filename="", overwrite=TRUE)
 }
if(grdlocal) grdfiles <- readLines("DWDdata/INDEX_of_DWD_grids.txt") else
 {
 messaget("Indexing FTP Server at gridbase...")
 grdfiles <- indexFTP("currentgindex", filename="grids", base=gridbase, overwrite=TRUE)
 } 
 
messaget("Calling createIndex...")
index <- createIndex(paths=dwdfiles, meta=TRUE, force=metaforce, overwrite=TRUE, checkwarn=FALSE)

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
urls <- urls[!  (grepl("1*_minute", urls) & !grepl("meta_data", urls))     ]
files <- dataDWD(urls, dir=localtestdir(), read=F)
rv <- readVars(files)
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
write.table(rv_df, "misc/params.txt", sep="\t", quote=F, row.names=F)
message("- Copy content of 'misc/params.txt' to 'params.xlsx'.
- Manually add'Kurz' entries.
- Copy to dwdparams in R/readVars.R")
}
#
# check for duplicates:
dupli <- rv[sapply(rv, function(x) sum(duplicated(x[,"Kurz"]))>0)]
if(length(dupli)!=0) print(dupli)
# check for new entries:
new <- which(sapply(rv, function(x)any(!x$Par %in% dwdparams$Parameter)))
if(length(new)!=0) print(new)


# Final messages ----

message(" - To view the checkIndex results, use   cat(.Last.value)")

a <- paste0("#' # ", formatC(length(dwdfiles), big.mark="'"), " (",Sys.Date(), ")")
b <- paste0("#' #  ",formatC(length(grdfiles), big.mark="'"), " (",Sys.Date(), ")")
message("- Manually add these file numbers to the examples in R/updateIndexes.R:\n", a, "\n", b)

dt <- Sys.time()-begintime
message("Done! updateIndex execution took ", round(dt,2), " ", attr(dt, "units"), ".")
return(invisible(index$checks))
}
