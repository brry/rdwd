#' @title local test data directory
#' @description returns a directory used for local tests on Berry's computers.
#' This is used in many examples to reduce the number of downloads by saving 
#' the downloaded DWD data in this directory.
#' My local test script is at \url{https://github.com/brry/rdwd/blob/master/misc/localtests.R}.
#' It runs all the examples, even the dontrun sections.
#' @return charstring (directory)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019
#' @keywords file
#' @importFrom berryFunctions packagePath
#' @export
#' @examples
#' localtestdir()
#'
#' @param packdir Path to package directory. DEFAULT: "."
#' @param folder  Path inside package. DEFAULT: "misc/localdata"
#' @param file    Optional: path(s) at \code{folder}. DEFAULT: NULL
#'
localtestdir <- function(
packdir=".",
folder="misc/localdata",
file=NULL
)
{
out <- berryFunctions::packagePath(packdir, warnonly=TRUE)
out <- paste0(out, "/", folder)
if(!is.null(file)) out <- paste0(out, "/", file)
out
}
