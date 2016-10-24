#' Select data from the DWD CDC FTP Server
#'
#' Select files for downloading with \code{\link{dataDWD}}.
#' All arguments (except for \code{mindex}, \code{findex} and \code{base})
#' can be a vecor and will be recycled to the maximum length of all arguments.
#' If that length > 1, the output is a list of filenames.\cr
#' If station \code{name} is given, but \code{id} is empty (""),
#' \code{id} is inferred via \code{mindex}.
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
#' selectDWD(id="00386", res="daily", var="kl", time="h") # time abbreviatable
#' selectDWD(id="00386", res="daily", var="kl", time="historical", meta=TRUE)
#'
#' selectDWD(id="01050", res="daily", var="kl", time=c("r","h")) #vectorizable
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl") # must give time
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", time="r")
#' # vectorization gives not the outer product, but elementwise comparison:
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", time=c("r","h"))
#'
#' selectDWD(id="01050") # all files in all paths matching id
#' selectDWD(id="00386", meta=TRUE)
#' selectDWD(res="daily", var="kl", time="recent") # id missing, thus meta<-TRUE
#'
#' # See if errors come as expected:
#' selectDWD(res="dummy", var="dummy") # should give an informative error
#' selectDWD(res="dummy") # this one, too
#' selectDWD(res="daily", var="kl") # must give time
#' }
#'
#' @param name  Char: station name that will be matched in \code{mindex} to obtain
#'              \code{id}. DEFAULT: ""
#' @param id    Char/Number: station ID with or without leading zeros, e.g. "00614" or 614.
#'              Is internally converted to an integer, because some DWD meta data
#'              Files also contain no leading zeros.
#'              If \code{id} is given, \code{name} is ignored. DEFAULT: ""
#' @param base  Single char: main directory of DWD ftp server, defaulting to
#'              observed climatic records.
#'              Must be the same \code{base} used to create \code{findex}.
#'              DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param res   Char: temporal resolution available at \code{base}, usually one of
#'              \code{c("hourly","daily","monthly")}. DEFAULT: ""
#' @param var   Char: Weather variable of interest, usually one of
#'              \code{c("kl", "more_precip", "soil_temperature", "solar",
#'              "air_temperature", "cloudiness", "precipitation", "pressure", "sun", "wind")}.
#'              See more in \code{View(rdwd:::\link{fileIndex})}. DEFAULT: ""
#' @param time  Char: desired time range. One of
#'              "recent" (data from the last year, up to date usually within a few days) or
#'              "historical" (long time series). Can be abbreviated (if the first
#'              letter is "r" or "h", full names are used. DEFAULT: ""
#' @param mindex Single object: Index used to select \code{id} if \code{name}
#'               is given. DEFAULT: \code{rdwd:::\link{metaIndex}}
#' @param findex Single object: Index used to select filename, as returned by
#'              \code{\link{index2df}}.To use a current / custom index, use
#'              \code{myIndex <- index2df(indexDWD("/daily/solar"))}
#'              (with desired path, of course). DEFAULT: \code{rdwd:::\link{fileIndex}}
#' @param meta  Logical: return metadata txt file name instead of climate data zip file?
#'              Relevant only in case 4 (path and id given). DEFAULT: FALSE
#' @param files Logical: for case 3/4 with given \code{path}: instead of station
#'              metadata, return a list of the currently available files?
#'              This will call code{\link{indexDWD}} with the path, thus
#'              requiring the \code{RCurl} package. DEFAULT: FALSE
#' @param ziponly Logical: If \code{files=TRUE}, only return the zip files, not the
#'              description (Beschreibung.txt/.pdf) files? DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{indexDWD}} if \code{files=TRUE}
#'
selectDWD <- function(
name="",
id="",
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
res="",
var="",
time="",
mindex=rdwd:::metaIndex,
findex=rdwd:::fileIndex,
meta=FALSE,
files=FALSE,
ziponly=TRUE,
...
)
{
# Input checks and processing:
findexname <- deparse(substitute(findex))
len <- max(length(name), length(id), length(res), length(var), length(time)  )
# recycle input vectors
if(len>1)
  {
  name    <- rep(name,   length.out=len)
  id      <- rep(id,     length.out=len)
  res     <- rep(res,    length.out=len)
  var     <- rep(var,    length.out=len)
  time    <- rep(time,   length.out=len)
  meta    <- rep(meta,   length.out=len)
  files   <- rep(files,  length.out=len)
  ziponly <- rep(ziponly,length.out=len)
}
# convert ID to integer:
id <- suppressWarnings(as.integer(id))
findex$id <- suppressWarnings(as.integer(findex$id))
# be safe for accidental vector input
base <- base[1]
# loop over each input element:
output <- lapply(seq_len(len), function(i)
{
# infer id from name if id=="" and name!="":
id.i <- id[i]
if(is.na(id.i) & name[i]!="")
  {
  id.i <- unique(mindex[mindex$Stationsname==name[i], "Stations_id"])
  }
if(length(id.i)!=1) warning("in selectDWD: determined ID is not of length 1, but ",
                         length(id.i), " (",toString(id.i), ").", call.=FALSE)
# cases (as in description)
givenid <- !is.na(id.i)
givenpath <- res[i]!="" & var[i]!="" # ignore time, because of var=solar possibility (no time)
# 1: id and path are both empty ------------------------------------------------
if(!givenid & !givenpath)
  {
  stop("in selectDWD: Neither station ID nor FTP folder is given.", call.=FALSE)
  # call.=FALSE to avoid uninformative  Error in FUN(X[[i]], ...) :
  # ToDo: decide what to do. Maybe some result from metaIndex
  }
# 2: id given, path empty ------------------------------------------------------
if(givenid & !givenpath)
  {
  if(meta) warning("selectDWD: meta is currently ignored if id is given", call.=FALSE)
  #ToDo: decide if there should instead of ignoring be some result from metaIndex
  filename <- findex[id.i==findex$id, "path"]
  filename <- filename[!is.na(filename)]
  return(   paste0(base, filename)   )
  }
#
# Case 3 and 4 (path given) - path existence check -----------------------------
time.i <- time[i]
if(substr(time.i,1,1)=="h") time.i <- "historical"
if(substr(time.i,1,1)=="r") time.i <- "recent"
if(var[i]=="solar") time.i <- ""
path <- paste0("/",res[i],"/",var[i],"/",time.i)
if(all(!grepl(path, findex$path))) stop("in selectDWD: According to findex '",
       findexname, "', the path '", path,
       "' doesn't exist. See ?selectDWD on how to use a different index.", call.=FALSE)
# select entry from findex:
sel <- res[i]==findex$res & var[i]==findex$var & time.i==findex$time
# # list available files:
if(files[i]) return(indexDWD(path, base=base, ziponly=ziponly[i], ...))
#
# 3: id is empty, path is given ------------------------------------------------
meta.i <- meta[i]
if(!givenid & givenpath) meta.i <- TRUE
# if either   case 3   or   4 with meta=TRUE  : return name of description txt file
if(meta.i)
  {
  sel <- sel & substr(findex$path, nchar(findex$path)-3, 1e4)==".txt"
  sel <- sel & grepl("Beschreibung", findex$path)
  # checks:
  if(sum(sel)==0) stop("in selectDWD: According to findex '",findexname,
                     "', there is no description file in '", path,
                     "'. See ?selectDWD on how to use a different index.", call.=FALSE)
  }
# 4: id and path are both given ------------------------------------------------
if(givenid & givenpath)
  {
  if(!meta.i) sel <- sel & sapply(id.i==findex$id, isTRUE)
  if(sum(sel)==0) stop("in selectDWD: According to findex '",findexname,
                       "', there is no file in '", path, "' with id '",id.i,
                       "'. See ?selectDWD on how to use a different index.", call.=FALSE)
  }
filename <- findex[sel,"path"]
if(length(filename)!=1) stop("in selectDWD: Several (or no) files were selected: ",
                             toString(filename), call.=FALSE)
return(   paste0(base, filename)   )
}) # loop end
output <- if(len==1) output[[1]] else output
return(output)
}
