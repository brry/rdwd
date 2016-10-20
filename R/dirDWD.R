#' directory management for rdwd
#'
#' Manage directories and files in the rdwd package.
#'
#' @name dirDWD
#' @aliases fileDWD
#' @return dirDWD invisibly returns the prior working directory as per \code{\link{setwd}}.\cr
#'         fileDWD returns the input with an added "_n" in the filename if the
#'         file already existed.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dataDWD}}
#' @keywords file
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @examples
#' # see source code of dataDWD and metaDWD
#'
#' @param dir Writeable directory. Created if not existent.
#'            DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param filename Character string with a single file name.
#' @param quiet Suppress message about directory / existing file? DEFAULT: FALSE
#'
dirDWD <- function(
dir="DWDdata",
quiet=FALSE
)
{
dir <- dir[1]
if(dir=="")
  {
  if(!quiet) message("dirDWD: no directory is created, getwd remains at '", getwd(), "'")
  return(getwd())
  }
#
if(!file.exists(dir))
  {
  dir.create(dir)
  if(!quiet) message("Created directory '",   normalizePath(dir, winslash="/"), "'")
  } else
  if(!quiet) message("Adding to directory '", normalizePath(dir, winslash="/"), "'")
setwd(dir)
}


#' @export
#' @rdname dirDWD


fileDWD <- function(
filename,
quiet=FALSE
)
{
f <- filename[1]
e2 <- tools::file_ext(f)
e1 <- tools::file_path_sans_ext(f)
Newfilecreated <- FALSE
nr <- 1
while(file.exists(f))
{
  f <- paste0(e1,"_",nr,".",e2)
  nr <- nr + 1
  Newfilecreated <- TRUE
}
if( Newfilecreated & !quiet) message("File already existed. Created the file '", f, "'")
if(!Newfilecreated & !quiet) message("Created the file '", f, "'")
f
}
