#' Select data from the DWD CDC FTP Server
#' 
#' Select files for downloading with \code{\link{dataDWD}}.
#' All arguments (except for \code{mindex}, \code{findex} and \code{base})
#' can be a vector and will be recycled to the maximum length of all arguments.
#' If that length > 1, the output is a list of filenames (or vector if \code{outvec=TRUE}).\cr
#' If station \code{name} is given, but \code{id} is empty (""),
#' \bold{id} is inferred via \code{mindex}.
#' If \code{res/var/per} are given and valid (existing in \code{findex}),
#' they are pasted together to form a \bold{path}.
#' Here is an overview of the behavior in each case of availability:
#' \tabular{llll}{
#' case \tab |  \bold{id} \tab |  \bold{path} \tab | output \cr
#'  1 \tab |  ""  \tab |  ""  \tab | \code{base} (and some warnings) \cr
#'  2 \tab | "xx" \tab |  ""  \tab | All file names (across paths) for station \bold{id} \cr
#'  3 \tab |  ""  \tab | "xx" \tab | The zip file names at \bold{path} \cr
#'  4 \tab | "xx" \tab | "xx" \tab | Regular single data file name \cr
#' }
#' For case 2, you can explicitly set \code{res="",var="",per=""} to avoid the 
#' default interactive selection.\cr
#' For case 3 and 4 (\bold{path} given), you can set \code{meta=TRUE}.
#' Then selectDWD will return the name of the station description file at \bold{path}.
#' This is why case 3 with \code{meta=FALSE} only returns the data file names (ending in .zip).\cr\cr\cr
#' The following folders in \bold{\code{res/var/per}} notation
#' (resolution/variable/period) are available at
#' \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/}\cr
#' "<" signifies a split into the folders \code{per} = "recent" and "historical".\cr
#' "<<" signifies a split into the folders \code{per} = "now", recent", "historical" and "meta_data".\cr
#' "-" signifies that there are no further sub-folders. \cr
#' Please note that both "solar" (-/<<) and "sun" (<) are available!
#' \tabular{lllll}{
#' \code{res}=\bold{10_minutes} \tab | \code{res}=\bold{hourly} \tab | \code{res}=\bold{daily} \tab | \code{res}=\bold{monthly} \tab | \code{res}=\bold{annual} \cr
#' \code{var=}            \tab                      \tab                      \tab                 \tab                 \cr
#'                        \tab |                    \tab | kl <               \tab | kl <          \tab | kl <          \cr
#'                        \tab |                    \tab | more_precip <      \tab | more_precip < \tab | more_precip < \cr
#' air_temperature <<     \tab | air_temperature <  \tab |                    \tab |               \tab |               \cr
#' extreme_temperature << \tab |                    \tab |                    \tab |               \tab |               \cr
#' extreme_wind <<        \tab |                    \tab |                    \tab |               \tab |               \cr
#'                        \tab | cloudiness <       \tab |                    \tab |               \tab |               \cr
#'                        \tab | cloud_type <       \tab |                    \tab |               \tab |               \cr
#' precipitation <<       \tab | precipitation <    \tab |                    \tab |               \tab |               \cr
#'                        \tab | pressure <         \tab |                    \tab |               \tab |               \cr
#'                        \tab | soil_temperature < \tab | soil_temperature < \tab |               \tab |               \cr
#' solar <<               \tab | solar -            \tab | solar -            \tab |               \tab |               \cr
#'                        \tab | sun <              \tab |                    \tab |               \tab |               \cr
#'                        \tab | visibility <       \tab |                    \tab |               \tab |               \cr
#'                        \tab |                    \tab | water_equiv <      \tab |               \tab |               \cr
#' wind <<                \tab | wind <             \tab |                    \tab |               \tab |               \cr
#' }
#' Please note that \code{1_minute/precipitation/historical} has subfolders for each year.
#' \tabular{lll}{
#' \code{res}=\bold{1_minute} \tab | \code{res}=\bold{multi_annual} \tab | \code{res}=\bold{subdaily} \cr 
#' \code{var=}      \tab                \tab                     \cr
#' precipitation << \tab |              \tab |                   \cr
#'                  \tab | mean_61-90 - \tab |                   \cr
#'                  \tab | mean_71-00 - \tab |                   \cr
#'                  \tab | mean_81-10 - \tab |                   \cr
#'                  \tab |              \tab | air_temperature < \cr
#'                  \tab |              \tab | pressure <        \cr
#'                  \tab |              \tab | standard_format - \cr
#' }
#' 
#' @return Character string with file path and name(s) in the format
#'         "base/res/var/per/filename.zip"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{metaIndex}}, \url{../doc/mapDWD.html},
#'          \code{vignette("mapDWD", package="rdwd")}
#' @keywords file
#' @importFrom berryFunctions truncMessage traceCall
#' @importFrom utils menu
#' @export
#' @examples
#' # Give weather station name (must be existing in metaIndex):
#' findID("Potsdam", exactmatch=FALSE)
#' selectDWD("Potsdam", res="daily", var="kl", per="historical")
#' # all files for all stations matching "Koeln":
#' selectDWD("Koeln", res="", var="", per="", exactmatch=FALSE)
#' findID("Koeln", FALSE)
#' 
#' \dontrun{ # Excluded from CRAN checks to save time
#' # or directly give station ID:
#' selectDWD(id="00386", res="daily", var="kl", per="historical")
#' selectDWD(id=386, res="daily", var="kl", per="historical")
#' # period abbreviatable:
#' selectDWD(id="00386", res="daily", var="kl", per="h")
#' selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE)
#' 
#' # vectorizable:
#' selectDWD(id="01050", res="daily", var="kl", per=c("r","h")) # list
#' selectDWD(id="01050", res="daily", var="kl", per="rh") # vector
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="r")
#' # vectorization gives not the outer product, but elementwise comparison:
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="hr")
#' 
#' # all zip files in all paths matching id:
#' selectDWD(id=c(1050, 386), res="",var="",per="")
#' # all zip files in a given path (if ID is empty):
#' head(  selectDWD(id="", res="daily", var="kl", per="recent")   )
#' 
#' # See if warnings come as expected and are informative:
#' selectDWD(res="",var="",per="")
#' selectDWD(7777, res="",var="",per="")
#' selectDWD(id=7777, res="",var="",per="")
#' selectDWD(id="", res="dummy", var="dummy", per="")
#' selectDWD(res="dummy", var="", per="")
#' selectDWD(res="daily", var="", per="r")
#' selectDWD(res="daily", var="kl", per="")
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="") # needs 'per'
#' selectDWD(id="00386", res="",var="",per="", meta=TRUE)
#' 
#' selectDWD("Potsdam", res="daily", var="solar")
#' # should be an error:
#' berryFunctions::is.error(  selectDWD(id="Potsdam", res="daily", var="solar"), TRUE)
#' berryFunctions::is.error(  selectDWD(id="", current=TRUE, res="",var="",per="") , tell=TRUE, force=TRUE)
#' }
#' 
#' @param name  Char: station name(s) passed to \code{\link{findID}}, along with
#'              \code{exactmatch} and \code{mindex}. 
#'              All 3 arguments are ignored if \code{id} is given. DEFAULT: ""
#' @param res   Char: temporal \bold{res}olution available at \code{base}, usually one of
#'              \code{c("hourly","daily","monthly")}, see section 'Description' above.
#'              \code{res/var/per} together form the \bold{path}. 
#'              DEFAULT: NA for interactive selection
#' @param var   Char: weather \bold{var}iable of interest, like e.g.
#'              \code{"air_temperature", "cloudiness", "precipitation",
#'                      "soil_temperature", "solar", "kl", "more_precip"}
#'              See above and in \code{View(rdwd:::\link{fileIndex})}. 
#'              DEFAULT: NA for interactive selection
#' @param per   Char: desired time \bold{per}iod. One of
#'              "recent" (data from the last year, up to date usually within a few days) or
#'              "historical" (long time series). Can be abbreviated (if the first
#'              letter is "r" or "h", full names are used). To get both datasets,
#'              use \code{per="hr"} or \code{per="rh"} (and \code{outvec=TRUE}).
#'              \code{per} is set to "" if var=="solar". 
#'              DEFAULT: NA for interactive selection
#' @param exactmatch Logical passed to \code{\link{findID}}: match \code{name}
#'              with \code{\link{==}})? Else with \code{\link{grepl}}. DEFAULT: TRUE
#' @param mindex Single object: Index with metadata passed to \code{\link{findID}}.
#'              DEFAULT: \code{rdwd:::\link{metaIndex}}
#' @param id    Char/Number: station ID with or without leading zeros, e.g. "00614" or 614.
#'              Is internally converted to an integer, because some DWD meta data
#'              files also contain no leading zeros. DEFAULT: findID(name, exaxtmatch, mindex)
#' @param base  Single char: main directory of DWD ftp server, defaulting to
#'              observed climatic records (\code{\link{dwdbase}}).
#'              Must be the same \code{base} used to create \code{findex}.
#'              DEFAULT: \url{ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate}
#' @param findex Single object: Index used to select filename, as returned by
#'              \code{\link{createIndex}}.To use a current / custom index, use
#'              \code{myIndex <- createIndex(indexFTP("/daily/solar"))}
#'              (with desired path, of course). DEFAULT: \code{rdwd:::\link{fileIndex}}
#' @param current Single logical for case 3/4 with given \code{path}: instead of
#'              \code{findex}, use a list of the currently available files at
#'              base/res/var/per? This will call \code{\link{indexFTP}}, thus
#'              requires availability of the \code{RCurl} package.
#'              DEFAULT: FALSE
#' @param meta  Logical: return metadata txt file name instead of climate data zip file?
#'              Relevant only in case 4 (path and id given) and case 3 for res="multi_annual".
#'              See \code{\link{metaIndex}} for a compilation of all metaData files.
#'              DEFAULT: FALSE
#' @param outvec Single logical: if \bold{path} or \bold{ID} length > 1,
#'              instead of a list, return a vector? (via \code{\link{unlist}}).
#'              DEFAULT: \code{per %in% c("rh","hr")}
#' @param \dots Further arguments passed to \code{\link{indexFTP}} if \code{current=TRUE},
#'              like dir, quiet
#' 
selectDWD <- function(
name="",
res=NA,
var=NA,
per=NA,
exactmatch=TRUE,
mindex=metaIndex,
id=findID(name, exactmatch=exactmatch, mindex=mindex),
base=dwdbase,
findex=fileIndex,
current=FALSE,
meta=FALSE,
outvec=any(per %in% c("rh","hr")),
...
)
{
# unused arguments:
unused <- names(list(...))
unused <- unused[!unused %in% names(formals(indexFTP))]
if(length(unused)>0) warning("unused arguments in ", berryFunctions::traceCall(1,"",""), ": ",
                             toString(unused), call.=FALSE)
# Input checks and processing:
findexname <- deparse(substitute(findex))
# time period insertations:
outvec <- outvec # needs to be evaluated before per is changed
if(any(per=="hr"|per=="rh", na.rm=TRUE))
  {
  p_index_hr <- which(per=="hr")
  p_index_hr <- p_index_hr + seq_along(p_index_hr)-1
  for(i in p_index_hr) per <- append(per, "r", after=i)
  per[per=="hr"] <- "h"
  p_index_rh <- which(per=="rh")
  p_index_rh <- p_index_rh + seq_along(p_index_rh)-1
  for(i in p_index_rh) per <- append(per, "h", after=i)
  per[per=="rh"] <- "r"
  rm(i)
  }
# res/var/per interactive selection:
selectPrompt <- function(question, column, res="", var="", index=findex)
 {
 if(all(var=="solar" & res %in% c("hourly","daily"))) return("")
 question <- paste("Which of the following", question, "would you like to use?")
 options <- index[,column]
 if(any(res!=""))
   options <- index[index$res %in% res, column]
 if(any(var!=""))
  options <- index[index$res %in% res & index$var %in% var, column]
 options <- sort(unique(options))
 if("hourly" %in% options) # order resolution manually:
   {
   names(options) <- options
   ord <- c("1_minute","10_minutes","hourly","subdaily","daily","monthly","annual")
   ord <- c(ord, options[!options %in% ord]) # potentially further resolutions
   options <- options[ord]
   names(options) <- NULL
   }
 #options <- c(options, "''")
 sel <- menu(options, title=question)
 if(sel==0) return("")
 options[sel]
}
if(anyNA(res)) res[is.na(res)] <- selectPrompt("resolutions", "res")
if(anyNA(var)) var[is.na(var)] <- selectPrompt("variables",   "var", res=res)
if(anyNA(per)) per[is.na(per)] <- selectPrompt("periods",     "per", res=res, var=var)
# 
# recycle input vectors
len <- max(length(id), length(res), length(var), length(per), length(meta)  )
if(len>1)
  {
  id   <- rep(id,   length.out=len)
  res  <- rep(res,  length.out=len)
  var  <- rep(var,  length.out=len)
  per  <- rep(per,  length.out=len)
  meta <- rep(meta, length.out=len)
}
# be safe from accidental vector input
base <- base[1]
# per partial matching (abbreviation):
per[substr(per,1,1)=="h"] <- "historical"
per[substr(per,1,1)=="r"] <- "recent"
# solar per to ""
per[var=="solar" & res %in% c("hourly","daily")] <- ""
# check ids for accidental letters:
idlett <- grepl("[A-Za-z]", id)
if(any(idlett)) stop(traceCall(1, "in ", ": "), "id may not contain letters: ",
                     toString(id[idlett]), call.=FALSE)
# update file index:
if(current)
  {
  uniquepaths <- unique(paste0("/",res,"/",var,"/",per))
  uniquepaths <- uniquepaths[uniquepaths!="///"]
  if(length(uniquepaths)<1) stop(traceCall(1, "in ", ": "),
                    "current=TRUE, but no valid paths available.", call.=FALSE)
  findex <- createIndex(indexFTP(uniquepaths, ...), fname="")
  findexname <- "currentIndex"
  }
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
givenpath <- res[i]!="" & var[i]!="" # ignore per, because of var=solar possibility (no per)
if(res[i]=="multi_annual") givenpath <- res[i]!=""
#
# 1: id and path are both empty ------------------------------------------------
# base + warning
if(!givenid & !givenpath)
  {
  warning(traceCall(3, "", ": "), "neither station ID nor valid FTP folder is given.", call.=FALSE)
  # call.=FALSE to avoid uninformative  Error in FUN(X[[i]], ...) :
  return(base)
  }
# 2: id given, path empty ------------------------------------------------------
# all file names for station ID, regardless of path
if(givenid & !givenpath)
  {
  if(meta[i]) warning(traceCall(3, "", ": "),
          "meta is ignored if id is given, but path is not given.", call.=FALSE)
  filename <- findex[findex$id %in% id[i], "path"]
  filename <- filename[!is.na(filename)]
  # check output length
  if(length(filename)<1) warning(traceCall(3, "", ": "), "in file index '", findexname,
                                 "', no filename could be detected with ID ",
                                 id[i], ".", call.=FALSE)
  if(length(filename)>1) warning(traceCall(3, "", ": "), "in file index '", findexname,
                                 "', there are ", length(filename), " files with ID ",
                                 id[i], ".", call.=FALSE)
  return(   paste0(base, filename)   )
  }
#
# Case 3 and 4 (path given) - path existence check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path <- paste0("/",res[i],"/",var[i],"/",per[i])
if(all(!grepl(path, findex$path))) warning(traceCall(3, "", ": "), "according to file index '",
       findexname, "', the path '", path, "' doesn't exist.", call.=FALSE)
if(res[i]=="multi_annual" & per[i]=="") {per[i] <- var[i]; var[i] <- ""}
# select entries from file index:
sel <- res[i]==findex$res & var[i]==findex$var & per[i]==findex$per
#
# case 3 or 4 with meta=TRUE
ismeta <- grepl('.txt$', findex$path) & grepl("Beschreibung", findex$path)
if(res[i]=="multi_annual") 
   ismeta <- grepl('.pdf$', findex$path) | grepl("Stationsliste", findex$path)
# return name of description txt file
if(meta[i])
  {
  sel <- sel & ismeta
  # checks:
  if(sum(sel)==0) warning(traceCall(3, "", ": "), "according to file index '",findexname,
                     "', there is no description file in '", path, "'.", call.=FALSE)
  filename <- findex[sel,"path"]
  return(   paste0(base, filename)   )
  }
# 3: id is empty, path is given ------------------------------------------------
# all filenames EXCEPT metadata (-> only zipfiles)
if(!givenid & givenpath & !meta[i])
  {
  isnotmeta <- grepl('.zip$', findex$path)
  if(res[i]=="multi_annual") isnotmeta <- !ismeta
  sel <- sel & isnotmeta 
  filename <- findex[sel,"path"]
  if(length(filename)<1) warning(traceCall(3, "", ": "), "according to file index '",
                                 findexname, "', there is no file in '", path,
                                 "' with ID ", id[i], ".", call.=FALSE)
  return(   paste0(base, filename)   )
  }
# 4: id and path are both given ------------------------------------------------
# regular single data file name
if(givenid & givenpath & !meta[i])
  {
  sel <- sel & findex$id %in% id[i]
  if(sum(sel)==0) warning(traceCall(3, "", ": "), "according to file index '",findexname,
                          "', there is no file in '", path, "' with ID ",
                          id[i], ".", call.=FALSE)
  filename <- findex[sel,"path"]
  if(length(filename)>1) warning(traceCall(3, "", ": "), "several files (",
                                 length(filename),") were selected:",
                                 berryFunctions::truncMessage(filename, prefix=""),
                                 call.=FALSE)
  return(   paste0(base, filename)   )
  }
}) # loop end
output <- if(len==1) output[[1]] else output
if(len>1 & outvec) output <- unlist(output)
return(output)
}
