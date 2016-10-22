#' Select data from the DWD CDC FTP Server
#'
#' Select files for downloading with \code{\link{dataDWD}}.
#' Each argument can be a vector, then the rules below are applied element-wise.\cr # ToDo: not yet
#' If station \code{name} is given, but \code{id} is empty (""),
#' \code{id} is inferred via \code{findex}.
#' # ToDo: fileIndex (instead of indexlist), metaIndex. argument names findex, mindex
#' If \code{res/var/time} are given and valid (existing in \code{findex}),
#' they are pasted together to form a \code{path}.
#' Here is an overview of the behaviour in each case of availability:
#' \tabular{llll}{
#' case \tab | id \tab | path \tab | action \cr
#'  1 \tab |  ""  \tab |  ""  \tab | ToDo decide, is now the DEFAULT \cr
#'  2 \tab | "xx" \tab |  ""  \tab | All file names (across paths) for station "id" \cr
#'  3 \tab |  ""  \tab | "xx" \tab | meta=TRUE -> name of station description file at path \cr
#'  4 \tab | "xx" \tab | "xx" \tab | regular single data file name \cr
#' }
#'
#' @return Character string with file path and name(s) in the format
#'         "base/res/var/time/filename.zip"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file
#' @export
#' @examples
#' \dontrun{
#' selectDWD(id="00386", res="daily", var="kl", time="historical")
#' selectDWD(id="00386")
#' selectDWD(id="00386", meta=TRUE)
#' selectDWD(res="daily", var="kl", time="recent")
#' selectDWD(var="dummy") # should give an informative error
#' selectDWD(time="dummy") # this one, too
#' }
#'
#' @param id,name Character strings. If both are "", \code{meta} is set to TRUE.
#'                If \code{id} is given, \code{name} is ignored. DEFAULT: ""
#' @param meta Logical: return metadata txt file name instead of climate data zip file?
#'             DEFAULT: FALSE
#' @param index Index used to select filename, as returned by
#'              \code{\link{index2df}}. To use a current / custom index, use
#'              \code{myIndex <- index2df(indexDWD("/daily/solar"))}
#'              (with desired path, of course). DEFAULT: rdwd:::indexlist
#' @param base Main directory of DWD ftp server, defaulting to observed climatic records.
#'             Must be the same \code{base} used to create \code{index}.
#'             DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}
#' @param res Temporal resolution available at \code{base}, usually one of
#'            \code{c("hourly","daily","monthly")}. DEFAULT: ""
#' @param var Weather variable of interest, usually one of
#'            \code{c("kl", "more_precip", "soil_temperature", "solar",
#'            "air_temperature", "cloudiness", "precipitation", "pressure", "sun", "wind")}.
#'            See more in \code{\link{indexlist}}. DEFAULT: ""
#' @param time Desired time range. One of
#'             "recent" (data from the last year, up to date usually within a few days) or
#'             "historical" (long time series). DEFAULT: ""
#' @param files Logical for case 3/4 with \code{meta}=TRUE: istead of station metadata,
#'              return a list of the currently available files?
#'              This will call code{\link{indexDWD}} with the path, thus
#'              requiring the \code{RCurl} package.  DEFAULT: FALSE
#' @param ziponly Logical: If \code{files=TRUE}, only return the zip files, not the
#'                description (Beschreibung.txt/.pdf) files? DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{indexDWD}} if \code{files=TRUE}
#'
selectDWD <- function(
id="",
name="",
meta=FALSE,
index=rdwd:::indexlist,
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
res="",
var="",
time="",
files=FALSE,
ziponly=TRUE,
...
)
{
# Input checks and processing:
indexname <- deparse(substitute(index))
id   <-   id[1]
name <- name[1]
meta <- meta[1]
base <- base[1] # toDo: recycle all to max length + lapply + progress bar
res  <-  res[1]
var  <-  var[1]
time <- time[1]
# infer id from name if id=="" and name!="":
# ToDo
# cases (as in description)
givenid <- id!=""
givenpath <- res!="" & var!="" # ignore time, because of var=solar possibility (no time)
# 1: id and path are both empty ------------------------------------------------
if(!givenid & !givenpath)
  {
  stop("Neither station ID nor FTP folder is given.")   # ToDo: decide what to do.
  }                                                     # Maybe some result from metaIndex
# 2: id given, path empty ------------------------------------------------------
if(givenid & !givenpath)
  {
  if(meta) warning("meta is currently ignored if id is given") #ToDo: decide what to do.
  filename <- index[id==index$id,"path"]                       # Maybe some result from metaIndex
  return(   paste0(base, filename)   )
  }
#
# Case 3 and 4 (path given) - path existence check -----------------------------
if(var=="solar") time <- ""
path <- paste0("/",res,"/",var,"/",time)
if(all(!grepl(path, index$path))) stop("According to index '",indexname,
      "', the path '", path,"' doesn't exist. See ?metaDWD on how to use a different index.")
# select entry from index:
sel <- index$res==res & index$var==var & index$time==time
# # list available files:
if(files) return(indexDWD(path, base=base, ziponly=ziponly, ...))
#
# 3: id is empty, path is given ------------------------------------------------
if(!givenid & givenpath) meta <- TRUE
# if either   case 3   or   4 with meta=TRUE  : return name of description txt file
if(meta)
  {
  sel <- sel & substr(index$path, nchar(index$path)-3, 1e4)==".txt"
  sel <- sel & grepl("Beschreibung", index$path)
  # checks:
  if(sum(sel)==0) stop("According to index '",indexname,
                     "', there is no description file in '", path,
                     "'. See ?metaDWD on how to use a different index.")
  }
# 4: id and path are both given ------------------------------------------------
if(givenid & givenpath)
  {
  sel <- sel & id==index$id
  if(sum(sel)==0) stop("According to index '",indexname,
                       "', there is no file in '", path, "' with id '",id,
                       "'. See ?metaDWD on how to use a different index.")
  }
filename <- index[sel,"path"]
if(length(filename)!=1) stop("Several (or no) files were selected: ", toString(filename))
return(   paste0(base, filename)   )
# toDo: loop end stuff
}
