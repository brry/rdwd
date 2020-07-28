# small helper functions, onload code, release questions
# turn on Rstudio Outline (CTRL + SHIFT + O) for overview

# * rdwdquiet ----

#' @title global quiet option for rdwd
#' @description global quiet option. The default `rdwdquiet()` is `FALSE`.\cr
#' Just write the following in your code and all subsequent calls will be quiet:\cr
#' `options(rdwdquiet=TRUE)`
#' @export
rdwdquiet <- function()
{
cv <- getOption("rdwdquiet", default=FALSE) # current value
if(!(isTRUE(cv)|isFALSE(cv))) stop("options('rdwdquiet') must be TRUE or FALSE, not '",
                                 toString(cv), "'.")
cv
}



.onLoad <- function(libname, pkgname)
{
options(rdwdquiet=FALSE)
invisible(NULL)
}



# * release_questions ----

#' @title Reminders when using devtools::release
#' @description  Reminders when using devtools::release.
#' @keywords internal

release_questions <- function() {
  c(
    "Have you run updateIndexes() and runLocalTests()?"
  )
}



# * dwdbase / gridbase ----

#' @title DWD FTP Server base URL
#' @aliases gridbase
#' @export
#' @description base URLs to the DWD FTP Server\cr\cr
#' **`dwdbase`**: observed climatic records at\cr
#' <ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate>\cr
#' An overview of available datasets and usage suggestions can be found at\cr
#' <https://bookdown.org/brry/rdwd/available-datasets.html>\cr
#' <https://bookdown.org/brry/rdwd/station-selection.html>\cr\cr\cr
#' **`gridbase`**: spatially interpolated gridded data at\cr
#' <ftp://opendata.dwd.de/climate_environment/CDC/grids_germany>\cr
#' Usage instructions can be found at\cr
#' <https://bookdown.org/brry/rdwd/raster-data.html>
#' 
dwdbase <- "ftp://opendata.dwd.de/climate_environment/CDC/observations_germany/climate"
#' @export
gridbase <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany"



# * rowDisplay ---------------------------------------------------------------------

#' Create leaflet map popup from data.frame rows
#' 
#' Create display character string for leaflet map popup from data.frame rows.
#' This function is not exported, as it is only internally useful.
#' A generic version is available in [berryFunctions::popleaf()].
#' 
#' @return Vector of character strings, one for each row in x.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2017
#' @seealso [`geoIndex`]
#' @keywords character
#' @importFrom berryFunctions removeSpace
#' 
#' @param x data.frame with colnames
#' 
rowDisplay <- function(
x
)
{
perrow <- function(x) paste0("rdwd::metaInfo(id=",removeSpace(x[1]),")<br>",
                             paste0(names(x)[-1], ": ", x[-1], collapse="<br>"))
apply(x, MARGIN=1, perrow)
}



# * checkSuggestedPackage --------------------------------------------------------

#' @title check suggested package for availability
#' @description check suggested package for availability,
#'              yielding an instructive error message if not
#' @return invisible success logical value from [requireNamespace()]
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso [requireNamespace()]
#' @keywords package
# @export # only needed internally, I think
#' 
#' @param package      Charstring: package to be checked for loadability
#' @param functionname Charstring: function name to be used in the message
#' 
checkSuggestedPackage <- function(package, functionname)
{
available <- requireNamespace(package, quietly=TRUE)
if(!available) stop("To use ",functionname, ", please first install ",
                    package,":    install.packages('",package,"')", call.=FALSE)
return(invisible(available))
}

