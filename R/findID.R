#' find DWD weather station ID from name
#' 
#' Identify DWD weather station ID from station name
#' 
#' @return Character string (vector) with ID(s)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct-Nov 2016
#' @seealso used in \code{\link{selectDWD}}, \code{\link{metaInfo}}
#' @keywords character
#' @export
#' @importFrom berryFunctions traceCall
#' @examples
#' # Give weather station name (must be existing in metaIndex):
#' findID("Potsdam")
#' findID("potsDam") # capitalization is ignored
#' # all names containing "Hamburg":
#' findID("Hamburg", exactmatch=FALSE)
#' findID("Potsdam", exactmatch=FALSE)
#' 
#' # vectorized:
#' findID(c("Potsdam","Berlin-Buch"))
#' 
#' # German Umlauts are changed to ue, ae, oe, ss
#' findID("Muenchen", FALSE)
#' berryFunctions::convertUmlaut("M?nchen") # use this to convert umlauts in lists
#' 
#' @param name  Char: station name(s) that will be matched in \code{mindex} to obtain
#'              \bold{id}. DEFAULT: ""
#' @param exactmatch Logical: Should \code{name} match an entry in \code{mindex}
#'              exactly (be \code{\link{==}})?
#'              If FALSE, \code{name} may be a part of \code{mindex$Stationsname},
#'              as checked with \code{\link{grepl}}. This is useful e.g. to get
#'              all stations starting with a name (e.g. 42 IDs for Berlin).
#'              DEFAULT: TRUE
#' @param mindex Single object: Index used to select \code{id} if \code{name}
#'              is given. DEFAULT: \code{rdwd:::\link{metaIndex}}
#' @param quiet Logical: suppress length warnings? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#'  
findID <- function(
name="",
exactmatch=TRUE,
mindex=metaIndex,
quiet=rdwdquiet()
)
{
# Input checks and processing:
len <- length(name)
exactmatch <- rep(exactmatch, length.out=len)
#
# ------------------------------------------------------------------------------
# loop over each input element:
output <- lapply(seq_len(len), function(i)
  {
  if(name[i]=="") return("")
  select <- if(exactmatch[i]) tolower(mindex$Stationsname)==tolower(name[i]) else
                              grepl(name[i], mindex$Stationsname, ignore.case=TRUE)
  id <- unique(mindex[select, "Stations_id"])
  # warn about length
  if(length(id)<1)
    {
    id <- NA
    if(!quiet) warning(traceCall(3, "", ": "), "no ID could be determined from name '",
                             name[i], "'.", call.=FALSE)
    }
  if(length(id)>1 & !quiet) warning(traceCall(3, "", ": "), "ID determined from name '",
                             name[i], "' has ", length(id), " elements (",
                             toString(sort(id)), ").", call.=FALSE)
  # ID names:
  names(id) <- mindex[ match(id, mindex$Stations_id), "Stationsname" ]
  id
  }) # loop end
# output:
return(unlist(output))
}
