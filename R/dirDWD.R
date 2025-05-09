#' directory management for rdwd
#' 
#' Manage directories with useful messages in the rdwd package.
#' 
#' @name dirDWD
#' @return dirDWD invisibly returns the prior working directory as per [setwd()].
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso [dataDWD()]
#' @keywords file
#' @importFrom berryFunctions tmessage
#' @examples
#' # see source code of dataDWD and metaDWD
#' 
#' @param dir      Char for dirDWD: writeable directory name. Created if not existent.
#'                 DEFAULT: [locdir()]
#' @param quiet    Logical: Suppress messages about creating dir? DEFAULT: FALSE through [rdwdquiet()]
#' 
dirDWD <- function(
dir=locdir(),
quiet=rdwdquiet()
)
{
dir <- dir[1]
# Remove trailing slashes to avoid Windows warning in dir.create(dir): 'folder' already exists
dir <- sub("[/|\\]+$", "", dir)
if(dir=="")
  {
  if(!quiet) tmessage("no directory is created, getwd remains at '", getwd(), "'")
  return(getwd())
  }
#
if(!file.exists(dir))
  {
  dir.create(dir, recursive=TRUE)
  if(!quiet) tmessage("creating directory '",   normalizePath(dir, winslash="/"), "'")
  } else
  if(!quiet) tmessage("adding to directory '", normalizePath(dir, winslash="/"), "'")
setwd(dir)
}
