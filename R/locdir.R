#' @title local data directory
#' @description This can be used to set a directory for DWD data across projects,
#' thus avoiding multiple downloads of the same file.\cr
#' Set the default for all subsequent calls with `options(rdwdlocdir="YOUR/PATH")`.\cr
#' You could add this to your .Rprofile file e.g. via `usethis::edit_r_profile()`\cr
#' @seealso [runLocalTests()]
#' @return charstring (directory)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019, Jun 2021
#' @keywords file
#' @importFrom berryFunctions packagePath twarning
#' @export
#' @examples
#' locdir()
#' oldopt <- options(rdwdlocdir="~")
#' locdir()
#' stopifnot(locdir() == path.expand("~"))
#' options(oldopt) ; rm(oldopt)
#' 
#' @param dir     Path to data directory. 
#'                If `dir` does not exist, [tempdir()] is used instead 
#'                (with a warning, unless `quiet=TRUE`).
#'                If `dir` is NULL, locdir tries "C:/DWDdata", then "~/DWDdata".\cr
#'                `dir` can also be set with 
#'                `options(rdwdlocdir="YOUR/PATH")` thanks to the 
#'                DEFAULT: [getOption]`("rdwdlocdir")`
#' @param file    Optional: path(s) at `dir`. DEFAULT: NULL
#' @param quiet   Logical: suppress tempdir warning? 
#'                DEFAULT: FALSE through [rdwdquiet()] 
#' 
locdir <- function(
dir=getOption("rdwdlocdir"),
file=NULL,
quiet=rdwdquiet()
)
{
if(is.null(dir)) dir <- "C:/DWDdata"
if(!file.exists(dir))dir <- "~/DWDdata"
if(!file.exists(dir))
  {
  if(!quiet) twarning("'", dir, "' does not exist, using tempdir() now.")
  dir <- tempdir()
  }
if(!is.null(file)) dir <- paste0(dir, "/", file)
dir <- path.expand(dir)
dir
}

