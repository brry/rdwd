#' directory management for rdwd
#'
#' Manage directories in the rdwd package
#'
#' @return prior working directory
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file
# @export
#' @examples
#' # see source code of dataDWD and metaDWD
#'
#' @param dir Writeable directory. Created if not existent.
#'        DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param quiet Suppress message about directory? DEFAULT: FALSE
#'
dirDWD <- function(
dir="DWDdata",
quiet=FALSE
)
{
if(!file.exists(dir))
  {
  dir.create(dir)
  if(!quiet) message("Directory '", dir, "' created at '", getwd(),"'")
  } else
  if(!quiet) message("Adding to directory '", dir, "' at '", getwd(),"'")
# toDo: check for relative vs absolute paths, adjust message accordingly
owd <- setwd(dir)
owd
}
