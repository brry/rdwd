#' @title Update rdwd development version
#' @description Update rdwd to the latest development version on github, if necessary.
#'         If the version number or date is larger on github, 
#'         \code{remotes::\link[remotes]{install_github}} will be called.
#' @return data.frame with version information
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2019
#' @seealso \code{\link{help}}, \code{remotes::\link[remotes]{install_github}}
#' @keywords file
#' @importFrom utils packageDescription
#' @export
#' @examples
#' # updateRdwd()
#'
#' @param pack     Name of (already installed) package. DEFAULT: "rdwd"
#' @param user     Github username. repo will then be user/pack. DEFAULT: "brry"
#' @param vignette build_vignettes in \code{remotes::\link[remotes]{install_github}}? 
#'                 DEFAULT: TRUE
#' @param quiet    Suppress version messages and \code{remotes::install} output?
#'                 DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots    Further arguments passed to \code{remotes::\link[remotes]{install_github}}
#'
updateRdwd <- function(
pack="rdwd",
user="brry",
vignette=TRUE,
quiet=rdwdquiet(),
...
)
{
# installed date/version:
Vinst <- suppressWarnings(utils::packageDescription(pack)[c("Date","Version")])
repo <- paste0(user,"/",pack)
# date/version in source code
url <- paste0("https://raw.githubusercontent.com/",repo,"/master/DESCRIPTION")
tf <- tempfile("DESCRIPTION")
download.file(url, tf, quiet=TRUE)
Vsrc <- read.dcf(file=tf, fields=c("Date","Version"))
Vsrc <- split(unname(Vsrc),colnames(Vsrc)) # transform matrix to list
output <- data.frame(Version=c(Vinst$Version, Vsrc$Version), 
                        Date=c(Vinst$Date,    Vsrc$Date))
rownames(output) <- paste0(pack,"_",c("Locally_installed", "Github_latest"))
# install if outdated:
doinst <-  Vsrc$Version > Vinst$Version   |   Vsrc$Date > Vinst$Date
if(!doinst)
{
if(!quiet) message(pack, " is up to date, compared to github.com/",repo,
         ". Version ", Vsrc$Version, " (", Vsrc$Date,")")
return(invisible(output))
}
if(!quiet) message(pack, " local version ", Vinst$Version, " (", Vinst$Date,
        ") is outdated.\nInstalling development version ", 
        Vsrc$Version, " (", Vsrc$Date,") from github.com/",repo)
checkSuggestedPackage("remotes", "updateRdwd")
# actually install, with vignettes (unlike remotes default)
remotes::install_github(repo=repo, build_vignettes=vignette, quiet=quiet, ...)
return(invisible(output))
}
