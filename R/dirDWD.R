#' directory management for rdwd
#' 
#' Manage directories with useful messages in the rdwd package.
#' 
#' @name dirDWD
#' @return dirDWD invisibly returns the prior working directory as per \code{\link{setwd}}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file
#' @importFrom berryFunctions traceCall
#' @examples
#' # see source code of dataDWD and metaDWD
#' 
#' @param dir      Char for dirDWD: writeable directory name. Created if not existent.
#'                 DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet    Logical: Suppress messages about creating dir? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' 
dirDWD <- function(
dir="DWDdata",
quiet=rdwdquiet()
)
{
dir <- dir[1]
# Remove trailing slashes to avoid Windows warning in dir.create(dir): 'folder' already exists
while(grepl("/$", dir))  dir <- sub("/$","",dir) 
if(dir=="")
  {
  if(!quiet) message(traceCall(1, "", ": "), "no directory is created, getwd remains at '", getwd(), "'")
  return(getwd())
  }
#
if(!file.exists(dir))
  {
  dir.create(dir, recursive=TRUE)
  if(!quiet) message(traceCall(1, "", ": "), "creating directory '",   normalizePath(dir, winslash="/"), "'")
  } else
  if(!quiet) message(traceCall(1, "", ": "), "adding to directory '", normalizePath(dir, winslash="/"), "'")
setwd(dir)
}
