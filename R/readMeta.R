#' Process data from the DWD CDC FTP Server
#' 
#' Read climate meta info textfiles in zip folders downloaded with \code{\link{dataDWD}}.
#' 
#' @return Invisible named list of data.frames; or a list of lists, if length(file)>1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, 2016 + March 2019
#' @seealso \code{\link{dataDWD}}, \code{\link{readVars}}, \code{\link{readDWD}}
#' @keywords file
#' @importFrom utils read.table unzip
#' @importFrom berryFunctions checkFile
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD
#' 
#' @param file   Char (vector): name(s) of the zip file(s) downloaded with \code{\link{dataDWD}},
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip"
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: TRUE
#' @param \dots  Further arguments passed to \code{\link{read.table}}
#' 
readMeta <- function(
file,
progbar=TRUE,
...
)
{
checkFile(file)
# Optional progress bar:
if(missing(progbar) & length(file)==1) progbar <- FALSE
if(progbar) lapply <- pbapply::pblapply
# loop over each filename:
output <- lapply(file, function(f)
{
# temporary unzipping directory
fn <- tools::file_path_sans_ext(basename(f))
exdir <- paste0(tempdir(),"/", fn)
unzip(f, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE), add=TRUE)
# all meta info text files in zip folder:
fm <- dir(exdir, pattern="*.txt", full.names=TRUE)
fm <- fm[substr(basename(fm),1,7)!="produkt"]
tabs <- base::lapply(fm, function(fi)
  {
  #tab <- XML::readHTMLTable(fi, stringsAsFactors=FALSE)[[1]]
  #tab <- paste(apply(tab,1,paste,collapse=";"), collapse="\n")
  #tab <- read.table(header=TRUE, sep=";",stringsAsFactors=FALSE, text=tab)
  nr <- readLines(fi) # number of rows
  nr <- sum(!substr(nr, 1, 7) %in% c("Legende", "generie"))
  # message("reading ", nr, " rows in ", normalizePath(fi, winslash="/"), "...")
  tab <- read.table(fi, sep=";", header=TRUE, nrows=nr-1, ...)
  tab
  })
names(tabs) <- basename(fm)
return(tabs)
}) # end lapply loop over files
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}
