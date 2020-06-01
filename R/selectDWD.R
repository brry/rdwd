#' @title Select data from the DWD CDC FTP Server
#' 
#' @description Select files for downloading with \code{\link{dataDWD}}.\cr
#' The available folders with datasets are listed at
#' \url{https://bookdown.org/brry/rdwd/available-datasets.html}.
#' To use an updated index (if necessary), see 
#' \url{https://bookdown.org/brry/rdwd/station-selection.html#fileindex}.\cr\cr
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
#' 
#' @return Character string with file path and name(s) in the format
#'         "base/res/var/per/filename.zip"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}, \code{\link{metaIndex}},
#'          \url{https://bookdown.org/brry/rdwd}
#' @keywords file
#' @importFrom berryFunctions truncMessage traceCall
#' @importFrom utils menu
#' @export
#' @examples
#' # Give weather station name (must be existing in metaIndex):
#' selectDWD("Potsdam", res="daily", var="kl", per="historical")
#' 
#' # all files for all stations matching "Koeln":
#' selectDWD("Koeln", res="", var="", per="", exactmatch=FALSE)
#' findID("Koeln", FALSE)
#' 
#' \dontrun{ # Excluded from CRAN checks to save time
#' 
#' # selectDWD("Potsdam") # interactive selection of res/var/per
#' 
#' # directly give station ID, can also be id="00386" :
#' selectDWD(id=386, res="daily", var="kl", per="historical")
#' 
#' # period can be abbreviated:
#' selectDWD(id="00386", res="daily", var="kl", per="h")
#' selectDWD(id="00386", res="daily", var="kl", per="h", meta=TRUE)
#' 
#' # vectorizable:
#' selectDWD(id="01050", res="daily", var="kl", per="rh") # list if outvec=F
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="r")
#' # vectorization gives not the outer product, but elementwise comparison:
#' selectDWD(id="01050", res=c("daily","monthly"), var="kl", per="hr")
#' 
#' # all zip files in all paths matching id:
#' selectDWD(id=c(1050, 386), res="",var="",per="")
#' # all zip files in a given path (if ID is empty):
#' head(  selectDWD(id="", res="daily", var="kl", per="recent")   )
#' 
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
#' @param quiet Suppress id length warnings? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param id    Char/Number: station ID with or without leading zeros, e.g. "00614" or 614.
#'              Is internally converted to an integer, because some DWD meta data
#'              files also contain no leading zeros. DEFAULT: findID(name, exaxtmatch, mindex)
#' @param base  Single char: main directory of DWD ftp server.
#'              Must be the same \code{base} used to create \code{findex}.
#'              DEFAULT: \code{\link{dwdbase}}
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
#' @param meta_txt_only Logical: if \code{meta}, only return .txt files, not the 
#'              pdf and html files? DEFAULT: TRUE
#' @param outvec Single logical: if \bold{path} or \bold{ID} length > 1,
#'              instead of a list, return a vector? (via \code{\link{unlist}}).
#'              DEFAULT: \code{per \%in\% c("rh","hr")}
#' @param \dots Further arguments passed to \code{\link{indexFTP}} if \code{current=TRUE},
#'              except folder and base.
#' 
selectDWD <- function(
name="",
res=NA,
var=NA,
per=NA,
exactmatch=TRUE,
mindex=metaIndex,
quiet=rdwdquiet(),
id=findID(name, exactmatch=exactmatch, mindex=mindex, quiet=quiet),
base=dwdbase,
findex=fileIndex,
current=FALSE,
meta=FALSE,
meta_txt_only=TRUE,
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
#
# recycle input vectors
len <- max(length(id), length(res), length(var), length(per), length(meta)  )
lmin <-  c(length(id), length(res), length(var), length(per), length(meta)  )
if(any(lmin==0)) stop(sum(lmin==0), " input vectors have length zero: ", 
                      toString(c("id","res","var","per","meta")[lmin==0]))
# outside of the loop, the slowest part of the code is getting length(id)
# because findID obtains id from name. All computing time happens in tolower
if(len>1)
  {
  id   <- rep(id,   length.out=len)
  res  <- rep(res,  length.out=len)
  var  <- rep(var,  length.out=len)
  per  <- rep(per,  length.out=len)
  meta <- rep(meta, length.out=len)
  meta_txt_only <- rep(meta_txt_only, length.out=len)
  }
# be safe from accidental vector input
base <- base[1]
# per partial matching (abbreviation):
per[substr(per,1,1)=="h"] <- "historical"
per[substr(per,1,1)=="r"] <- "recent"
# solar per to ""
per[var=="solar" & res %in% c("hourly","daily")] <- ""
# multiannual data has no id, remove if given:
rma <- res=="multi_annual"
if(any(rma, na.rm=TRUE))
  {
  if(any(id[rma]!="", na.rm=TRUE)) warning(traceCall(1, "", ": "), "multi_annual data is not ", 
      "organized by station ID. Setting id to ''.", call.=FALSE)
  id[rma] <- ""
  if(any(per[rma]!="", na.rm=TRUE)) warning(traceCall(1, "", ": "), "multi_annual data is not ", 
      "organized in period folders. Setting per to ''.", call.=FALSE)
  per[rma] <- ""
  }
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
  findex <- createIndex(indexFTP(folder=uniquepaths, base=base, quiet=quiet, ...), fname="")
  findexname <- "currentIndex"
  }
# convert ID to integer:
id <- suppressWarnings(as.integer(id))
#
# res/var/per interactive selection:
selectPrompt <- function(column, RES="", VAR="", PER="", ID="", index=findex)
 {
 # input arguments capitalized to avoid restarting interrupted promise evaluation
  ID[is.na(ID )] <- ""
 RES[is.na(RES)] <- ""
 VAR[is.na(VAR)] <- ""
 PER[is.na(PER)] <- ""
 if(all(VAR=="solar" & RES %in% c("hourly","daily"))) return("")
 question <- if(column=="res") "resolutions" else 
             if(column=="var") "variables"   else "periods"
 question <- paste("Which of the following", question, "would you like to use?")
 sid  <- if(any(ID !="")) index$id  %in% ID  else TRUE
 sres <- if(any(RES!="")) index$res %in% RES else TRUE
 svar <- if(any(VAR!="")) index$var %in% VAR else TRUE
 sper <- if(any(PER!="")) index$per %in% PER else TRUE
 options <- index[sid & sres & svar & sper , column]
 options <- sort(unique(options))
 if(length(options)<1) 
   {
   warning("For interactive selection, no options were found for your request. ",
           "Proceeding with ", column,"=''.", call.=FALSE)
   return("")
   }
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
if(anyNA(res)) res[is.na(res)] <- selectPrompt("res", res, var, per, id)
if(anyNA(var)) var[is.na(var)] <- selectPrompt("var", res, var, per, id)
if(anyNA(per)) per[is.na(per)] <- selectPrompt("per", res, var, per, id)
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
  if(length(filename)<1 & !quiet) warning(traceCall(3, "", ": "), "in file index '", 
     findexname, "', no filename could be detected with ID ", id[i], ".", call.=FALSE)
  if(length(filename)>1 & !quiet) warning(traceCall(3, "", ": "), "in file index '", 
     findexname, "', there are ", length(filename), " files with ID ",id[i], ".", 
     call.=FALSE)
  return(   paste0(base,"/",filename)   )
  }
#
# Case 3 and 4 (path given) - path existence check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path <- paste0("/",res[i],"/",var[i],"/",per[i])
if(res[i]=="multi_annual" & per[i]=="") {per[i] <- var[i]; var[i] <- ""}
# select entries from file index:
sel <- res[i]==findex$res & var[i]==findex$var & per[i]==findex$per
# within the loop, computing time seems to come from the selection
if(sum(sel)<1) warning(traceCall(3, "", ": "), "according to file index '",
           findexname, "', the path '", path, "' doesn't exist.", call.=FALSE)
#
# case 3 or 4 with meta=TRUE
# return name of description txt file
if(meta[i])
  {
  sel <- sel & findex$ismeta
  if(meta_txt_only[i]) sel <- sel & ! grepl(".pdf$", findex$path)
  # checks:
  if(sum(sel)==0) warning(traceCall(3, "", ": "), "according to file index '",findexname,
                     "', there is no description file in '", path, "'.", call.=FALSE)
  filename <- findex[sel,"path"]
  return(   paste0(base,"/",filename)   )
  }
# 3: id is empty, path is given ------------------------------------------------
# all filenames EXCEPT metadata (-> only zipfiles)
if(!givenid & givenpath & !meta[i])
  {
  isnotmeta <- grepl('.zip$', findex$path)
  if(res[i]=="multi_annual") isnotmeta <- !findex$ismeta
  sel <- sel & isnotmeta 
  filename <- findex[sel,"path"]
  if(length(filename)<1) warning(traceCall(3, "", ": "), "according to file index '",
                                 findexname, "', there is no file in '", path,
                                 "' with ID ", id[i], ".", call.=FALSE)
  return(   paste0(base,"/",filename)   )
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
  return(   paste0(base,"/",filename)   )
  }
}) # loop end
output <- if(len==1) output[[1]] else output
if(len>1 & outvec) output <- unlist(output)
return(output)
}
