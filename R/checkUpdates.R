#' @title check some historical files for updates by DWD
#' @description check whether the DWD has updated historical datasets.
#' That requires [`updateIndexes`] to be run by me.
#' If that is the case, the funtion will give a warning, otherwise a message.
#' @return currently available files on the FTP server in the ca 34 historical folders, invisibly
#' @export
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar+May 2025
#' @seealso <https://bookdown.org/brry/rdwd/fileindex.html>
#' @examples
#' # current <- checkUpdates()
#' @param fast only check a subset? currently ignored. DEFAULT: TRUE
checkUpdates <- function(
  fast=TRUE
  )
{
# Are the files from the index still present on the DWD server? ----------------
 
# historical files in the index:
# findex <- rdwd:::fileIndex # for development
findex <- fileIndex
# select recent historical files:
findex <- findex[!grepl("minute",findex$res),]
findex <- findex[!grepl("indices",findex$var),]
findex <- findex[findex$end>=Sys.Date()-600,] # only historical files have an end date
findex <- findex[!is.na(findex$end),]
findex$rvp <- paste(findex$res, findex$var, findex$per, sep="/")

# currently avalaible files:
current <- indexFTP(unique(findex$rvp), nosave=TRUE)

# check:
# # Artifical outdated index for code development:
# sel <- findex$res %in% c("hourly","daily") & findex$var %in% c("dew_point","kl")
# findex$path[sel] <- sub("20241231","20231231",findex$path[sel]) ; rm(sel)
findex$present <- findex$path %in% current 
# display only one file per folder
gone <- findex$path[!findex$present]
gone <- gone[!duplicated(dirname(gone))]
mOK <- "The historical index files are fully present on the DWD server :)."
if(all(findex$present)) message(mOK) else warning(
   sum(!findex$present)," filenames from the fileIndex are no longer on the DWD server.",
   "\nDisplaying one per folder:\n- ", paste(gone, collapse="\n- "), 
   "\nProbably, the DWD updated historical files to end last year.",
   "\nBerry needs to run   rdwd:::updateIndexes()")

# Have the historical folders been updated by the DWD? -------------------------

cindex <- createIndex(current, dir=tempdir(), fname="", quiet=TRUE)
cindex$rvp <- paste(cindex$res, cindex$var, cindex$per, sep="/")
old <- tapply(cindex$end, cindex$rvp, max, na.rm=TRUE) < Sys.Date()-366
mOK <- "The historical folders are updated on the DWD server."
if(!any(old)) message(mOK) else warning(
  "The DWD has not yet updated the historical files in ", sum(old),"/",length(old),
  " folders:\n- ", paste(names(old)[old], collapse="\n- ")) 

# output:
return(invisible(current))
}
