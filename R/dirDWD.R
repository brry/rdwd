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
#' fns <- c("data", "stupiddummy", "ExampleGraph.png", "rdwd.Rproj", "README.md",
#'          "stupiddummy.txtdude", "DESCRIPTION", "test_devel.R")
#' fileDWD(fns)
#' fileDWD(fns, ignore=TRUE)
#' fileDWD(fns, ignore=rep(0:1, each=4))
#'
#' @param dir      Char for dirDWD: writeable directory name. Created if not existent.
#'                 DEFAULT: "DWDdata" at current \code{\link{getwd}()}
#' @param filename Char (vector) for fileDWD: file name(s).
#' @param ignore   Logical (vector) for fileDWD: Ignore file? Handy in dataDWD. DEFAULT: FALSE
#' @param quiet    Logical: Suppress messages about creating dir / file(s)? DEFAULT: FALSE
#'
dirDWD <- function(
dir="DWDdata",
quiet=FALSE
)
{
dir <- dir[1]
if(dir=="")
  {
  if(!quiet) message("rdwd::dirDWD: no directory is created, getwd remains at '", getwd(), "'")
  return(getwd())
  }
#
if(!file.exists(dir))
  {
  dir.create(dir)
  if(!quiet) message("rdwd::dirDWD: creating directory '",   normalizePath(dir, winslash="/"), "'")
  } else
  if(!quiet) message("rdwd::dirDWD: adding to directory '", normalizePath(dir, winslash="/"), "'")
setwd(dir)
}


#' @export
#' @rdname dirDWD


fileDWD <- function(
filename,
quiet=FALSE,
ignore=FALSE
)
{
ignore <- rep(ignore, length.out=length(filename))
output <- lapply(seq_along(filename), function(i)
{
f <- filename[i]
if(ignore[i]) return(c(NA, f))
#
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
  if(isTRUE(any(!existed))) message("rdwd::fileDWD: creating the file",
    if(sum(!existed,na.rm=TRUE)>1) "s", " '", toString(fnames[sapply(!existed,isTRUE)]), "'")
  if(isTRUE(any( existed))) message("rdwd::fileDWD: file",
    if(sum( existed,na.rm=TRUE)>1) "s", " already existed. Creating the file",
    if(sum( existed,na.rm=TRUE)>1) "s", " '", toString(fnames[sapply( existed,isTRUE)]), "'")
  }
fnames
}
