#' Process data from the DWD CDC FTP Server
#' 
#' Read climate variables (column meta data) from files downloaded with \code{\link{dataDWD}}.
#' The metadata file is read, processed and returned as a data.frame.\cr
#' \code{file} can be a vector with several filenames. 
#' 
#' @return Invisible data.frame of the desired dataset, or a list of data.frames
#'         if length(file) > 1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso \code{\link{dataDWD}}, \code{\link{readDWD}}, \code{\link{parameter_abbreviations}}
#' @keywords file
#' @importFrom utils read.table unzip
#' @importFrom berryFunctions checkFile na9
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' \dontrun{ ## Excluded from CRAN checks
#' link <- selectDWD("Potsdam", res="daily", var="kl", per="recent")
#' file <- dataDWD(link, read=FALSE, dir=tempdir())
#' clim <- readDWD(file)
#' vars <- readVars(file)
#' 
#' }
#' 
#' @param file   Char (vector): name(s) of the file(s) downloaded with \code{\link{dataDWD}},
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip" 
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: TRUE
#' 
readVars <- function(
file,
progbar=TRUE
)
{
# Optional progress bar:
if(missing(progbar) & length(file)==1) progbar <- FALSE
if(progbar) lapply <- pbapply::pblapply
#
checkFile(file)
#
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
f <- file[i]

# temporary unzipping directory
fn <- tools::file_path_sans_ext(basename(f))
exdir <- paste0(tempdir(),"/", fn)
unzip(f, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE), add=TRUE)
f <- dir(exdir, pattern="Metadaten_Parameter.*txt", full.names=TRUE)
if(length(f)!=1) stop(traceCall(3, "", ": "), "The number of determined ",
   "'Metadaten_Parameter*.txt' files should be 1, but is ", length(f), ".\n",
   if(grepl("10_minutes", fn)) c("10-minute data does not have meta-information ",
                                 "in the zip folders as of 2018-06.") else 
    c("Please contact berry-b@gmx.de with reproducible code leading to this error.",
                      "\nAt least include the DWD path: ", fn), call.=FALSE)
nr <- readLines(f) # number of rows
nr <- sum(!substr(nr, 1, 7) %in% c("Legende", "generie"))
tab <- read.table(f, na.strings=na9(), sep=";", header=TRUE, nrows=nr-1, 
                  stringsAsFactors=FALSE)

tab <- tab[,c("Parameter", "Parameterbeschreibung", "Einheit")]
tab <- unique(tab)

dupli <- duplicated(tab$Parameter)
if(any(dupli)) warning(traceCall(3, "", ": "), "The following entries are",
                       " duplicated: ", toString(unique(tab$Parameter[dupli])),
                       "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'",
                       call.=FALSE)
rownames(tab) <- NULL

# Merge with short variable descriptions:
tab2 <- merge(parameter_abbreviations, tab, all.y=TRUE)
kurzna <- is.na(tab2$Kurz)
if(any(kurzna)) warning(traceCall(3, "", ": "), "The following entries are not",
                        " abbreviated yet: ", toString(tab2$Parameter[kurzna]),
                        "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'.",
                        "\nPlease inform berry-b@gmx.de so this can be included.",
                        call.=FALSE)

colnames(tab2)[1] <- "Par"
rownames(tab2) <- tab2$Par

# return column metadata:
return(tab2)
# lapply loop end
})
#
#
output <- if(length(file)==1) output[[1]] else output
return(invisible(output))
}


