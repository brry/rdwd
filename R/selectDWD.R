#' @title Select data from the DWD CDC FTP Server
#' 
#' @description Select data files for downloading with [dataDWD()].\cr
#' The available `res/var/per` folders with datasets are listed 
#' [online](https://bookdown.org/brry/rdwd/available-datasets.html).\cr
#' Set `res="", var="", per=""` to avoid the default interactive selection.\cr
#' The arguments `name/id` and `res/var/per` can be vectors.
#' 
#' @return Character string with file path and name(s) in the format
#'         "base/res/var/per/filename.zip"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016, rewritten May 2022
#' @seealso [dataDWD()], [`metaIndex`],
#'          [website station selection chapter](https://bookdown.org/brry/rdwd/station-selection.html#by-name)
#' @keywords file
#' @importFrom berryFunctions truncMessage twarning tstop
#' @importFrom utils menu
#' @importFrom stats na.omit
#' @export
#' @examples
#' # Give weather station name (must exist in metaIndex):
#' selectDWD("Potsdam", res="daily", var="kl", per="historical")
#' 
#' # all files for all stations matching "Koeln":
#' tail(selectDWD("Koeln", res="", var="", per="", exactmatch=FALSE)) # 686 files
#' findID("Koeln", FALSE)
#' 
#' \dontrun{ # Excluded from CRAN checks to save time
#' 
#' # selectDWD("Potsdam") # interactive selection of res/var/per
#' 
#' # directly give station ID:
#' selectDWD(id="00386", res="daily", var="kl", per="historical")
#' selectDWD(id=537, "", "", "", "") # 8 files
#' 
#' # period can be abbreviated:
#' selectDWD(id="5419", res="daily", var="kl", per="h")
#' 
#' # selectDWD is vectorizable!
#' # since version 1.5.28 (2022-05-12) outer product, not elementwise comparison:
#' selectDWD("Freiburg", res="daily", var="kl", per="rh")
#' selectDWD("Freiburg", res=c("daily","monthly"), var="kl", per="r")
#' selectDWD("Freiburg", res=c("daily","monthly"), var="kl", per="hr")
#' # get old behaviour (needed e.g. in nearbyStations):
#' ids <- c(3761,3761, 3603)
#' # all combinations:
#' selectDWD(id=ids, res="daily", var="kl", per=c("h","r","r")) # 4
#' # only given combinations:
#' selectDWD(id=ids, res="daily", var="kl", per=c("h","r","r"), expand=FALSE) # 3
#' 
#' # all files in all paths matching id:
#' head( selectDWD(id=c(1050, 386), res="",var="",per="") ) # 277 files
#' # all files in a given path (if ID is empty):
#' head(  selectDWD(id="", res="daily", var="kl", per="recent")   ) # 585 files
#' 
#' selectDWD(id=386, res="monthly", var="kl", per="h")
#' 
#' # Meta  data - Description and Beschreibung txt/pdf files.:
#' # manually select .txt (not pdf) files for automated opening with readDWD.
#' link <- selectDWD(res="monthly", var="kl", per="h", meta=TRUE) # omit ID/Name!
#' link
#' link2 <- grep("\\.txt$", link, value=TRUE)   ;   link2
#' m <- dataDWD(link2, dir=locdir())
#' head(m)
#' # 
#' # Open PDF files with your system's default Viewer:
#' dataDWD(link[1], dir=locdir())
#' }
#' 
#' @param name  Char: station name(s) passed to [findID()], along with
#'              `exactmatch`, `mindex` and `failempty`.
#'              All 3 arguments are ignored if `id` is given. 
#'              DEFAULT: "" (all stations at `res/var/per`)
#' @param res   Char: temporal **res**olution at `base`, e.g.
#'              `"hourly","daily","monthly"`.
#'              See section 'Description' above and [`fileIndex`].
#'              Use `res=""` for matching options from all resolutions.
#'              DEFAULT: NA for interactive selection
#' @param var   Char: weather **var**iable of interest, e.g.
#'              `"air_temperature", "cloudiness", "precipitation", "solar", "kl"`.
#'              See section 'Description' above and [`fileIndex`].
#'              DEFAULT: NA for interactive selection
#' @param per   Char: desired time **per**iod, e.g.
#'              "recent" (up to date records from the last 1.5 years) or
#'              "historical" (long time series). 
#'              Can be abbreviated. To get both datasets, use `per="hr"`.
#'              DEFAULT: NA for interactive selection
#' @param expand Logical: get all possible `res/var/per` combinations? 
#'              Set to FALSE if you want only the given combinations.
#'              If FALSE, they cannot be NA or "".
#'              DEFAULT: TRUE
#' @param id    Char/Number: station ID with or without leading zeros, e.g. "00614" or 614.
#'              Is internally converted to an integer. 
#'              Use NA (the default from `findID`) to get all data at `res/var/per`.
#'              DEFAULT: [`findID`]`(name, exaxtmatch, mindex, failempty)`
#' @param exactmatch Logical passed to [findID()]: match `name`
#'              with [`==`])? Else with [grepl()]. DEFAULT: TRUE
#' @param mindex Single object: Index with metadata passed to [findID()].
#'              DEFAULT: [`metaIndex`]
#' @param failempty Fail if no matching station is found in [findID()]? 
#'              Avoid downloading all files. DEFAULT: TRUE
#' @param findex Single object: Index used to select filename, as returned by
#'              [createIndex()].To use a current / custom index, see `current` and
#'              <https://bookdown.org/brry/rdwd/fileindex.html>. 
#'              DEFAULT: [`fileIndex`]
#' @param current Single logical when `res/var/per` is given: instead of
#'              `findex`, use a list of the currently available files at
#'              base/res/var/per? This will call [indexFTP()], thus
#'              requires availability of the `RCurl` package.
#'              See <https://bookdown.org/brry/rdwd/fileindex.html>. 
#'              DEFAULT: FALSE
#' @param base  Single char: main directory of DWD ftp server.
#'              Must be the same `base` used to create `findex`.
#'              `sub("ftp:", "https:", dwdbase)` works fine.
#'              DEFAULT: [`dwdbase`]
#' @param meta  Logical: select Beschreibung file from `ismeta` entries in `findex`? 
#'              See [`metaIndex`] for a compilation of all Beschreibung files.
#'              See the 'Examples' section for handling pdf and txt files.
#'              DEFAULT: FALSE
#' @param quiet Suppress id length warnings? DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots Further arguments passed to [indexFTP()] if `current=TRUE`,
#'              except folder and base.
#' 
selectDWD <- function(
name="",
res=NA,
var=NA,
per=NA,
expand=TRUE,
id=findID(name, exactmatch=exactmatch, mindex=mindex, quiet=quiet, failempty=failempty),
exactmatch=TRUE,
mindex=metaIndex,
failempty=TRUE,
findex=fileIndex,
current=FALSE,
base=dwdbase,
meta=FALSE,
quiet=rdwdquiet(),
...
)
{
# Input checks -----------------------------------------------------------------
# unused arguments:
unused <- names(list(...))
unused <- unused[!unused %in% names(formals(indexFTP))]
if(length(unused)>0) twarning("unused arguments: ", toString(unused))
# misc:
findexname <- deparse(substitute(findex))
base <- base[1] # be safe from accidental vector input
force(id)
# check ids for accidental letters:
idlett <- grepl("[A-Za-z]", id)
if(any(idlett)) tstop("id may not contain letters: ", toString(id[idlett]))
# convert ID to integer (some DWD meta data files have no leading zeros): 
id <- suppressWarnings(as.integer(id)) # NA for name=""
# stop if id and path are both empty:
if(all(is.na(id)) && all(res==""|is.na(res)) && all(var==""|is.na(var)) && all(per==""|is.na(per)))
 tstop("One (or both) of 'id' and 'res/var/per' must be given.")

# res/var/per processing -------------------------------------------------------
# period rh:
if(any(per=="hr"|per=="rh", na.rm=TRUE)) per <- c("r","h")
per[substr(per,1,1)=="h"] <- "historical" # partial matching (abbreviation)
per[substr(per,1,1)=="r"] <- "recent"
# some solar data has no per:
per[var=="solar" & res %in% c("hourly","daily")] <- ""
# multiannual data has no id and var:
rma <- res=="multi_annual"
if(any(rma, na.rm=TRUE))
 {
 if(any(id[rma]!=""|var[rma]!="", na.rm=TRUE))
  twarning("Setting id and var to '' for multi_annual data. Use per to choose period.")
 id[rma] <- NA
 var[rma] <- ""
 }
if(!expand)
 {
 if(any(is.na(res)|res=="")) tstop("With expand=FALSE, 'res' may not be NA or '' (empty).")
 if(any(is.na(var)|var=="")) tstop("With expand=FALSE, 'var' may not be NA or '' (empty).")
 if(any(is.na(per)|per=="")) tstop("With expand=FALSE, 'per' may not be NA or '' (empty).")
 }
# update file index if wanted --------------------------------------------------
if(current)
  {
  uniquepaths <- unique(paste0("/",res,"/",var,"/",per))
  uniquepaths <- uniquepaths[uniquepaths!="///"]
  if(length(uniquepaths)<1) tstop("current=TRUE, but no valid paths available.")
  findex <- createIndex(indexFTP(folder=uniquepaths, base=base, quiet=quiet, ...), fname="")
  findexname <- "currentIndex"
  }

# helper functions -------------------------------------------------------------
formatquery <- function() paste0(
              "id='",paste( id,collapse="|"), 
          "', res='",paste(res,collapse="|"), 
          "', var='",paste(var,collapse="|"), 
          "', per='",paste(per,collapse="|"), "'", if(meta)", meta=TRUE")
 
# res/var/per (interactive) selection:
select <- function(v) # v = res / var / per
 {
 column <- deparse(substitute(v))
 if(!all(is.na(v))) 
  {
  if(all(v=="", na.rm=TRUE)) return(sel) else
  return(sel & findex[,column] %in% na.omit(v))
  }
 options <- sort(unique(findex[sel, column]))
 if(length(options)<1)
   {
   twarning("For interactive selection, no options were found for your request\n  ",
            formatquery(), ".\n  Proceeding with ", column,"=''.")
   return(sel)
   }
 ord <- c("1_minute","5_minutes","10_minutes","hourly","subdaily","daily","monthly","annual")
 if(any(ord %in% options)) # order res manually:
   {
   names(options) <- options
   ord2 <- c(ord, options[!options %in% ord]) # potentially further resolutions
   options <- na.omit(options[ord2])
   names(options) <- NULL
   }
 question <- switch(column,
  res="resolutions",
  var="variables",
  per="periods",
  id ="ids")
 question <- paste("Which of the following", question, "would you like to use?\n0: all of the below")
 selopt <- menu(options, title=question)
 if(selopt==0) return(sel)
 return(sel & findex[,column] %in% na.omit(options[selopt]))
 }

# Actual selection -------------------------------------------------------------
if(expand)
{
  sel <- if(meta) findex$ismeta else !findex$ismeta
  if(!all(is.na(id))) sel <- sel & findex$id  %in% na.omit(id)
  sel <- select(res)
  sel <- select(var)
  sel <- select(per)
} else
{
  len <- max(length(res),length(var),length(per),length(id))
  if(any(grepl("minute",res))) twarning("with expand=FALSE, only the first match",
                                        " is selected for 1/5/10-minute-data.")
  res <- rep(res, length.out=len)
  var <- rep(var, length.out=len)
  per <- rep(per, length.out=len)
  id  <- rep(id , length.out=len)
  sel <- sapply(1:len, function(i) which(findex$res==res[i] & 
                                         findex$var==var[i] & 
                                         findex$per==per[i] & 
                                         findex$id == id[i])[1])
}

# checks + output:
if(sum(sel)<1 & !quiet) 
 twarning("No entries in file index '", findexname, "' match your query\n  ",
          formatquery(), ".")
return(paste0(base,"/",findex[sel,"path"]))
}
