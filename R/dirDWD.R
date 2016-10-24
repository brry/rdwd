#' directory management for rdwd
#'
#' Manage directories and files in the rdwd package.
#' Files are never overwritten but saved with a message as OriginalName_1.file
#' or OriginalName_2.file etc.
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
#' @param dir      Char: writeable directory name. Created if not existent.
#'                 DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param filename Char (vector): for fileDWD only: file name(s).
#' @param quiet    Logical: Suppress messages about creating dir / file(s)? DEFAULT: FALSE
#'
dirDWD <- function(
dir="DWDdata",
#reading=FALSE,
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
  if(!quiet) message("Creating directory '",   normalizePath(dir, winslash="/"), "'")
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
output <- lapply(filename, function(f)
{
e2 <- tools::file_ext(f)
if(e2!="") e2 <- paste0(".",e2)
e1 <- tools::file_path_sans_ext(f)
existed <- FALSE
nr <- 1
while(file.exists(f))
{
  f <- paste0(e1,"_",nr,e2)
  nr <- nr + 1
  existed <- TRUE
}
return(c(existed, f))
})
fnames  <- sapply(output, "[", 2)
existed <- sapply(output, "[", 1)
existed <- as.logical(existed)
if(!quiet)
  {
  if(any(!existed)) message("Creating the file", if(sum(!existed)>1) "s",
                            " '", toString(fnames[!existed]), "'")
  if(any(existed)) message("File", if(sum(existed)>1) "s",
                           " already existed. Creating the file", if(sum(existed)>1) "s",
                            " '", toString(fnames[existed]), "'")
  }
fnames
}
