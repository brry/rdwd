#' file management for rdwd
#'
#' Manage files in the rdwd package.
#' Files are never overwritten but saved with a message as OriginalName_1.file
#' or OriginalName_2.file etc.
#'
#' @name fileDWD
#' @return fileDWD returns the input with an added "_n" in the filename if the
#'         file already existed.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Oct 2016
#' @seealso \code{\link{dirDWD}}, \code{\link{dataDWD}}
#' @keywords file
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom berryFunctions truncMessage traceCall
#' @export
#' @examples
#' # see source code of dataDWD and metaDWD
#'
#' fns <- c("data", "stupiddummy", "ExampleGraph.png", "rdwd.Rproj", "README.md",
#'          "stupiddummy.txtdude", "DESCRIPTION", "test_devel.R")
#' fileDWD(fns)
#' fileDWD(fns, ignore=TRUE)
#' fileDWD(fns, ignore=rep(0:1, each=4))
#' fileDWD(fns, ntrunc=2)
#' fileDWD("ExampleGraph.png")
#'
#' @param filename Char (vector): file name(s).
#' @param ignore   Logical (vector): Ignore file? Handy in dataDWD. DEFAULT: FALSE
#' @param quiet    Logical (vector): Suppress messages about creating file(s)? DEFAULT: FALSE
#' @param ntrunc   Integer: Number of filenames printed in messages before they
#'                 get truncated with message "(and xx more)". DEFAULT: 3
#'
fileDWD <- function(
filename,
quiet=FALSE,
ntrunc=3,
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
  # message names:
  n_e <- sum(existed, na.rm=TRUE) # number of existing files
  n_n <- sum(!is.na(existed)) # number of new files
  n_i <- sum(is.na(existed)) # number of ignored files
  message(traceCall(1, "", ": "),
          if(n_i>0) paste0("ignoring ", n_i, " file", if(n_i>1) "s", "; "),
          if(n_n>0) paste0("creating ", n_n, " file", if(n_n>1) "s"),
          if(n_e>0) paste0(" (",n_e," already existed for which '_n' is appended)"),
          ":", truncMessage(fnames, ntrunc=ntrunc, prefix=""))
  }
fnames
}
