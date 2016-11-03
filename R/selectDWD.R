#' Select data from the DWD CDC FTP Server
#'
#' Select files for downloading with \code{\link{dataDWD}}.
#' All arguments (except for \code{mindex}, \code{findex} and \code{base})
#' can be a vecor and will be recycled to the maximum length of all arguments.
#' If that length > 1, the output is a list of filenames (or vector with \code{outvec=TRUE}).\cr
#' If station \code{name} is given, but \code{id} is empty (""),
#' \bold{id} is inferred via \code{mindex}.
#' If \code{res/var/time} are given and valid (existing in \code{findex}),
#' they are pasted together to form a \bold{path}.
#' Here is an overview of the behaviour in each case of availability:
#' \tabular{llll}{
#' case \tab |  \bold{id} \tab |  \bold{path} \tab | output \cr
#'  1 \tab |  ""  \tab |  ""  \tab | \code{base} (and some warnings) \cr
#'  2 \tab | "xx" \tab |  ""  \tab | All file names (across paths) for station \bold{id} \cr
#'  3 \tab |  ""  \tab | "xx" \tab | The zip file names at \bold{path} \cr
#'  4 \tab | "xx" \tab | "xx" \tab | Regular single data file name \cr
#' }
#' For case 3 and 4 (\bold{path} given), you can set \code{meta=TRUE}.
#' Then selectDWD will return the name of the station description file at \bold{path}.
#' This is why case 3 with \code{meta=FALSE} only returns the data file names (ending in .zip).
#'
#' @return Character string with file path and name(s) in the format
#'         "base/res/var/time/filename.zip"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file
#' @export
#' @examples
#' # Give weather station name (must be existing in metaIndex):
#' findID("Potsdam", exactmatch=FALSE)
#' selectDWD("Potsdam", res="daily", var="kl", time="historical")
#' # all files for all stations matching "Koeln":
#' selectDWD("Koeln", exactmatch=FALSE)
#' findID("Koeln", FALSE)
#'
#' # or directly give station ID:
#' selectDWD(id="00386", res="daily", var="kl", time="historical")
#' selectDWD(id=386, res="daily", var="kl", time="historical")
#' # time abbreviatable:
#' selectDWD(id="00386", res="daily", var="kl", time="h")
#' selectDWD(id="00386", res="daily", var="kl", time="h", meta=TRUE)
#'
#' # vectorizable:
#' selectDWD(id="01050", res="daily", var="kl", time=c("r","h"))
#' selectDWD(id="01050", res="daily", var="kl", time=c("r","h"), outvec=TRUE)
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", time="r")
#' # vectorization gives not the outer product, but elementwise comparison:
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", time=c("r","h"))
#'
#' # all zip files in all paths matching id:
#' selectDWD(id=c(1050, 386))
#' # all zip files in a given path (if ID is empty):
#' head(  selectDWD(id="", res="daily", var="kl", time="recent")   )
#'
#' # See if warnings come as expected and are informative:
#' selectDWD()
#' selectDWD(7777)
#' selectDWD(id=7777)
#' selectDWD(id="", res="dummy", var="dummy")
#' selectDWD(res="dummy")
#' selectDWD(res="daily", time="r")
#' selectDWD(res="daily", var="kl")
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl") # needs 'time'
#' selectDWD(id="00386", meta=TRUE)
#'
#' selectDWD("Potsdam", res="daily", var="solar")
#' # should be an error:
#' berryFunctions::is.error(  selectDWD(id="Potsdam", res="daily", var="solar"), TRUE)
#' berryFunctions::is.error(  selectDWD(id="", current=TRUE) , tell=TRUE, force=TRUE)
#'
#'
#' @param name  Char: station name(s) passed to \code{\link{findID}}, along with the
#'              next two arguments. All ignored if \code{id} is given. DEFAULT: ""
#' @param exactmatch Logical passed to \code{\link{findID}}: match \code{name}
#'              with \code{\link{==}})? Else with \code{\link{grepl}}. DEFAULT: TRUE
#' @param mindex Single object: Index with metadata passed to \code{\link{findID}}.
#'              DEFAULT: \code{rdwd:::\link{metaIndex}}
#' @param id    Char/Number: station ID with or without leading zeros, e.g. "00614" or 614.
#'              Is internally converted to an integer, because some DWD meta data
#'              files also contain no leading zeros. DEFAULT: findID(name)
#' @param base  Single char: main directory of DWD ftp server, defaulting to
#'              observed climatic records.
#'              Must be the same \code{base} used to create \code{findex}.
#'              DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param res   Char: temporal resolution available at \code{base}, usually one of
#'              \code{c("hourly","daily","monthly")}, see \code{\link{rdwd}}.
#'              \code{res/var/time} together form the \bold{path}. DEFAULT: ""
#' @param var   Char: weather variable of interest, see \code{\link{rdwd}}. Usually one of
#'              \code{c("air_temperature", "cloudiness", "precipitation",
#'                      "pressure", "sun", "wind")} (only in hourly),
#'              \code{c("soil_temperature", "solar")} (in hourly and daily), or
#'              \code{c("kl", "more_precip")} (in daily and monthly).
#'              See more in \code{View(rdwd:::\link{fileIndex})}. DEFAULT: ""
#' @param time  Char: desired time range. One of
#'              "recent" (data from the last year, up to date usually within a few days) or
#'              "historical" (long time series). Can be abbreviated (if the first
#'              letter is "r" or "h", full names are used).
#'              Is set to "" if var=="solar". DEFAULT: ""
#' @param findex Single object: Index used to select filename, as returned by
#'              \code{\link{index2df}}.To use a current / custom index, use
#'              \code{myIndex <- index2df(indexDWD("/daily/solar"))}
#'              (with desired path, of course). DEFAULT: \code{rdwd:::\link{fileIndex}}
#' @param current Single logical for case 3/4 with given \code{path}: instead of
#'              \code{findex}, use a list of the currently available files at
#'              base/res/var/time? This will call \code{\link{indexDWD}}, thus
#'              requires availability of the \code{RCurl} package.
#'              DEFAULT: FALSE
#' @param meta  Logical: return metadata txt file name instead of climate data zip file?
#'              Relevant only in case 4 (path and id given). DEFAULT: FALSE
#' @param outvec Single logical: if \bold{path} or \bold{ID} length > 1,
#'              instead of a list, return a vector? (via \code{\link{unlist}}).
#'              DEFAULT: FALSE
#' @param \dots Further arguments passed to \code{\link{indexDWD}} if \code{current=TRUE},
#'              like dir, quiet
#'
selectDWD <- function(
name="",
exactmatch=TRUE,
mindex=metaIndex,
id=findID(name, exactmatch=exactmatch, mindex=mindex),
base="ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
res="",
var="",
time="",
findex=fileIndex,
current=FALSE,
meta=FALSE,
outvec=FALSE,
...
)
{
# Input checks and processing:
findexname <- deparse(substitute(findex))
len <- max(length(id), length(res), length(var), length(time), length(meta)  )
# recycle input vectors
if(len>1)
  {
  id      <- rep(id,     length.out=len)
  res     <- rep(res,    length.out=len)
  var     <- rep(var,    length.out=len)
  time    <- rep(time,   length.out=len)
  meta    <- rep(meta,   length.out=len)
}
# be safe from accidental vector input
base <- base[1]
# time partial matching (abbreviation):
time[substr(time,1,1)=="h"] <- "historical"
time[substr(time,1,1)=="r"] <- "recent"
# solar time to ""
time[var=="solar"] <- ""
# update file index:
if(current)
  {
  uniquepaths <- unique(paste0("/",res,"/",var,"/",time))
  uniquepaths <- uniquepaths[uniquepaths!="///"]
  if(length(uniquepaths)<1) stop("in rdwd::selectDWD: current=TRUE, but no valid ",
                                 "paths available.", call.=FALSE)
  findex <- index2df(indexDWD(uniquepaths, ...), filename="")
  findexname <- "currentIndex"
  }
# check ids for accidental letters:
idlett <- grepl("[A-Za-z]", id)
if(any(idlett)) stop("in rdwd::selectDWD: id may not contain letters: ",
                     toString(id[idlett]), call.=FALSE)
# convert ID to integer:
id <- suppressWarnings(as.integer(id))
findex$id <- suppressWarnings(as.integer(findex$id))
#
# ------------------------------------------------------------------------------
#
# loop over each input element:
output <- lapply(seq_len(len), function(i)
{
# ------------------------------------------------------------------------------
# cases (as in description)
givenid <- !is.na(id[i])
givenpath <- res[i]!="" & var[i]!="" # ignore time, because of var=solar possibility (no time)
#
# 1: id and path are both empty ------------------------------------------------
# base + warning
if(!givenid & !givenpath)
  {
  warning("in rdwd::selectDWD: Neither station ID nor FTP folder is given.", call.=FALSE)
  # call.=FALSE to avoid uninformative  Error in FUN(X[[i]], ...) :
  return(base)
  }
# 2: id given, path empty ------------------------------------------------------
# all file names for station ID, regardless of path
if(givenid & !givenpath)
  {
  if(meta[i]) warning("in rdwd::selectDWD: meta is ignored if id is given, but path is not given.", call.=FALSE)
  filename <- findex[findex$id %in% id[i], "path"]
  filename <- filename[!is.na(filename)]
  # check output length
  if(length(filename)<1) warning("in rdwd::selectDWD: in file index '", findexname,
                                 "', no filename could be detected with ID ",
                                 id[i], ".", call.=FALSE)
  if(length(filename)>1) warning("in rdwd::selectDWD: in file index '", findexname,
                                 "', there are ", length(filename), " files with ID ",
                                 id[i], ".", call.=FALSE)
  return(   paste0(base, filename)   )
  }
#
# Case 3 and 4 (path given) - path existence check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path <- paste0("/",res[i],"/",var[i],"/",time[i])
if(all(!grepl(path, findex$path))) warning("in rdwd::selectDWD: According to file index '",
       findexname, "', the path '", path, "' doesn't exist.", call.=FALSE)
# select entries from file index:
sel <- res[i]==findex$res & var[i]==findex$var & time[i]==findex$time
#
# case 3 or 4 with meta=TRUE
# return name of description txt file
if(meta[i])
  {
  sel <- sel & substr(findex$path, nchar(findex$path)-3, 1e4)==".txt"
  sel <- sel & grepl("Beschreibung", findex$path)
  # checks:
  if(sum(sel)==0) warning("in rdwd::selectDWD: According to file index '",findexname,
                     "', there is no description file in '", path, "'.", call.=FALSE)
  filename <- findex[sel,"path"]
  return(   paste0(base, filename)   )
  }
# 3: id is empty, path is given ------------------------------------------------
# all filenames EXCEPT metadata (-> only zipfiles)
if(!givenid & givenpath & !meta[i])
  {
  sel <- sel & substr(findex$path, nchar(findex$path)-3, 1e4)==".zip"
  filename <- findex[sel,"path"]
  if(length(filename)<1) warning("in rdwd::selectDWD: According to file index '",
                                 findexname, "', there is no file in '", path,
                                 "' with ID ", id[i], ".", call.=FALSE)
  return(   paste0(base, filename)   )
  }
# 4: id and path are both given ------------------------------------------------
# regular single data file name
if(givenid & givenpath & !meta[i])
  {
  sel <- sel & findex$id %in% id[i]
  if(sum(sel)==0) warning("in rdwd::selectDWD: According to file index '",findexname,
                          "', there is no file in '", path, "' with ID ",
                          id[i], ".", call.=FALSE)
  filename <- findex[sel,"path"]
  if(length(filename)>1) warning("in rdwd::selectDWD: Several files were selected: ",
                                 toString(filename), call.=FALSE)
  return(   paste0(base, filename)   )
  }
}) # loop end
output <- if(len==1) output[[1]] else output
if(len>1 & outvec) output <- unlist(output)
return(output)
}
