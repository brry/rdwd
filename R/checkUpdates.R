#' @title check some historical files for updates by DWD
#' @description check whether the DWD has updated historical datasets.
#' That requires [`updateIndexes`] to be run by me.
#' If that is the case, the funtion will give a warning, otherwise a message.
#' @return currently available files on the FTP server in the ca 34 historical folders, invisibly
#' @export
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2025
#' @seealso <https://bookdown.org/brry/rdwd/fileindex.html>
#' @examples
#' # current <- checkUpdates()
#' @param fast only check a subset? currently ignored. DEFAULT: TRUE
checkUpdates <- function(
  fast=TRUE
  )
{
# findex <- rdwd:::fileIndex # for development
findex <- fileIndex
# select recent historical files:
findex <- findex[!grepl("minute",findex$res),]
findex <- findex[!grepl("indices",findex$var),]
findex <- findex[findex$end>=Sys.Date()-600,]
findex <- findex[!is.na(findex$end),]
# select one file per historical folder:
findex$rvp <- paste(findex$res, findex$var, findex$per, sep="/")
sel <- function(x) if(length(x)==1) x else sample(x,1) # otherwise single entries are missing
findex <- findex[tapply(1:nrow(findex), findex$rvp, sel),]
# currently avalaible files:
current <- indexFTP(findex$rvp, nosave=TRUE)
# check for presence:
present <- findex$path %in% current
msg1 <- "The following filenames from the index are not (no longer) on the DWD server:\n- "
msg2 <- "\nProbably, the DWD updated historical files to end last year."
msg3 <- "\nBerry needs to run   rdwd:::updateIndexes()"
if(any(!present)) warning(msg1, paste(findex$path[!present], collapse="\n- "), msg2, msg3) else
                  message("The checked index file selection is fully present on the DWD server :).")
# output:
return(invisible(current))
}
