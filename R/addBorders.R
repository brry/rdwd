# addBorders -------------------------------------------------------------------

#' @title add country and Bundesland borders to a map
#' @return invisible list with DEU and EUR
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @seealso \code{\link{DEU}}, \code{\link{EUR}}
#' @keywords aplot
#' @export
#' @examples
#' if(requireNamespace("raster", quietly=TRUE)){
#' plot(1, xlim=c(2,16), ylim=c(47,55)) 
#' addBorders()
#' plot(1, xlim=c(2,16), ylim=c(47,55))
#' addBorders(de="orange", eu=NA)
#' }
#'
#' @param de      Color for Bandeslaender line. NA to suppress. DEFAULT: "grey80"
#' @param eu      Color for countries line. NA to suppress. DEFAULT: "black"
#' @param add     Logical: add to existing plot? DEFAULT: TRUE
#' @param \dots   Further arguments passed to \code{raster::\link[raster]{plot}}
addBorders <- function(de="grey80", eu="black", add=TRUE, ...)
{
checkSuggestedPackage("raster", "addBorders") # stops also if sp is missing
load(system.file("extdata/DEU.rda", package="rdwd"), envir=environment())
load(system.file("extdata/EUR.rda", package="rdwd"), envir=environment())
raster::plot(DEU, add=add, border=de, ...) 
raster::plot(EUR, add=TRUE, border=eu, ...)
return(invisible(list(DEU=DEU, EUR=EUR)))
}
# Suppress CRAN check note 'no visible binding for global variable':
if(getRversion() >= "2.15.1")  utils::globalVariables(c("DEU", "EUR"))


# DEU Map dataset --------------------------------------------------------------

#' Map of German states (Bundeslaender) from GADM through the \code{raster} package
#' @name DEU
#' @seealso \code{\link{addBorders}}, \code{\link{EUR}}
#' @details Obtained with the code: \cr
#' \code{DEU1 <- raster::getData("GADM", country="DEU", level=1)}\cr
#' \code{DEU <- rgeos::gSimplify(DEU1, tol=0.02, topologyPreserve=FALSE)}\cr
#' \code{raster::plot(DEU1)}\cr
#' \code{raster::plot(DEU)}\cr
#' \code{save(DEU,        file="inst/extdata/DEU.rda")}\cr
#' \code{tools::resaveRdaFiles("inst/extdata/DEU.rda")}\cr
#' @docType data
#' @format Formal class 'SpatialPolygons' [package "sp"] with 4 slots
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2018
#' @keywords datasets
if(requireNamespace("sp", quietly=TRUE))
load(system.file("extdata/DEU.rda", package="rdwd"), envir=environment())
# To have it avaliable as rdwd:::DEU


# EUR Map dataset --------------------------------------------------------------

#' Map of Western European countries through the \code{rworldmap} package
#' @name EUR
#' @seealso \code{\link{addBorders}}, \code{\link{DEU}}
#' @details Obtained with the code: \cr
#' \code{EUR <- rworldmap::getMap("low")}\cr
#' \code{EUR <- raster::crop(EUR, c(-5,20, 40,60)) }\cr
#' \code{raster::plot(EUR)}\cr
#' \code{save(EUR,        file="inst/extdata/EUR.rda", version=2)}\cr
#' \code{tools::resaveRdaFiles("inst/extdata/EUR.rda", version=2)}\cr
#' @docType data
#' @format SpatialPolygonsDataFrame [package "sp"] with 32 rows
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019
#' @keywords datasets
if(requireNamespace("sp", quietly=TRUE))
load(system.file("extdata/EUR.rda", package="rdwd"), envir=environment())

