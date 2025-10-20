#' @title local data directory
#' @description Get the directory for DWD data across projects,
#' thus avoiding multiple downloads of the same file.\cr
#' Set the default for all subsequent calls with `options(rdwdlocdir="YOUR/PATH")`.\cr
#' You could add this to your .Rprofile file e.g. via `usethis::edit_r_profile()`\cr
#' @seealso [dataDWD()], [runLocalTests()]
#' @return charstring (directory)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019, Jun 2021, Oct 2025
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
#'                If `dir` is NULL, locdir tries "C:/DWDdata", then "~/DWDdata",
#'                then [tools::R_user_dir]`("rdwd", which="cache")`.\cr\cr
#'                `dir` can also be set with 
#'                `options(rdwdlocdir="YOUR/PATH")` thanks to the 
#'                DEFAULT: [getOption]`("rdwdlocdir")`
#' @param file    Optional: path(s) at `dir`. DEFAULT: NULL
#' @param quiet   Ignored since version 1.9.4 (2025-10-20).
#'                DEFAULT: FALSE through [rdwdquiet()] 
#' 
locdir <- function(
dir=getOption("rdwdlocdir"),
file=NULL,
quiet=rdwdquiet()
)
{
if(is.null(dir)) dir <- "C:/DWDdata"
if(!file.exists(dir)) dir <- "~/DWDdata"
if(!file.exists(dir)) dir <- tools::R_user_dir("rdwd", which="cache")
if(!is.null(file)) dir <- paste0(dir, "/", file)
dir <- path.expand(dir)
dir
}

