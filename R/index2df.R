#' process the index of the DWD CDC FTP Server
#'
#' Create a data.frame out of the vector index returned by \code{\link{indexDWD}}.
#' \code{index2df} tries to obtain res, var, time, file, id, start and end from the paths.
#' This produces the \code{\link{indexlist}} used in \code{\link{selectDWD}}. ToDO: create and document 'metaIndex'
#'
#' @return invisible dataframe with a number of columns tried to infer from the paths.
#'         This is also saved to disc.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{indexDWD}}, \code{\link{indexlist}}, \code{\link{selectDWD}}
#' @keywords manip
#' @importFrom berryFunctions l2df
#' @importFrom utils write.table
#' @export
#' @examples
#' # see indexDWD
#'
#' @param path Vector of paths returned by \code{\link{indexDWD}} called with
#'             the default \code{base} value.
#' @param dir Writeable directory name where to save the output.
#'            Created if not existent. DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress messages about directory / filename? DEFAULT: FALSE
#'
index2df <- function(
path,
dir="DWDdata",
quiet=FALSE
)
{
# All paths should have the same amount of levels before being splitted:
index <- gsub("solar/", "solar//", path)
index <- gsub("multi_annual/", "multi_annual//", index)
index <- gsub("subdaily/standard_format/", "subdaily/standard_format//", index)
# remove leading slashes:
index <- ifelse(substr(index,1,1)=="/", substr(index,2,1e4), index)
# split into parts:
index <- l2df(lapply(index,function(x) strsplit(x,"/")[[1]]))
# ToDo: check if there are actually 4 columns (might be different with non-standard base)
colnames(index) <- c("res","var","time","file")
file <- index$file
index <- index[,1:3]
#
# Get detailed info from file name elements:
info <- l2df(lapply(file, function(x) rev(strsplit(x, "[-_.]")[[1]])))
#paths <- paste(index$res,index$var,index$time, sep="/")
#sel <- sapply(unique(paths), function(x) which(paths==x)[10])
#sortDF(index[sel,], time)
#info[sel, ]
#index[sel,]
# Station ID (identification number):
index$id <- ""
index$id <- ifelse(index$time=="historical" & info[,1]=="zip", info[,5], index$id)
index$id <- ifelse(index$time=="recent"     & info[,1]=="zip", info[,3], index$id)
index$id <- ifelse(index$var=="solar"       & info[,1]=="zip", info[,2], index$id) # var==solar
index$id <- ifelse(substr(file,1,2)=="kl", substr(file,4,8), index$id) # res==subdaily
#
# start and end of time series (according to file name):
ziphist <- index$time=="historical"  & info[,1]=="zip"
multi <- index$res=="multi_annual" & info[,1]=="txt" & info[,3]!="Stationsliste"
# actual selection:
index$start <- ""
index$start <- ifelse(ziphist, info[,4], index$start)
index$start <- ifelse(multi,   info[,4], index$start)
# Analogous for end:
index$end <- ""
index$end <- ifelse(ziphist, info[,3], index$end)
index$end <- ifelse(multi,   info[,3], index$end)
#
# Append path for accurate file reading later on, e.g. with dataDWD:
index$path <- path
#
# Write to disc
owd <- dirDWD(dir, quiet=quiet)
on.exit(setwd(owd))
outfile <- fileDWD("INDEX.txt", quiet=quiet)
write.table(index, file=outfile, sep="\t", row.names=FALSE, quote=FALSE)
# Output
return(invisible(index))
}
