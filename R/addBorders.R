# addBorders -------------------------------------------------------------------

#' @title add country and Bundesland borders to a map
#' @return invisible list with DEU and EUR
#' @details
#' ```
#' # Use the SpatVectors directly with:
#' DEU <- terra::vect(system.file("extdata/DEU.gpkg", package="rdwd"))
#' EUR <- terra::vect(system.file("extdata/EUR.gpkg", package="rdwd"))
#' 
#' # Obtained with the code:
#' url <- "https://gisco-services.ec.europa.eu/distribution/v2/nuts/shp/NUTS_RG_03M_2021_4326_LEVL_1.shp.zip"
#' tf <- tempfile(fileext=".zip")
#' download.file(url, tf) # 0.9 MB # in 2023-06 error 'Transferred a partial file'
#' unzip(tf, exdir="misc/vign") ; rm(url, tf)
#' 
#' DEU <- terra::vect("misc/vign/NUTS_RG_03M_2021_4326_LEVL_1.shp")
#' library(terra)
#' DEU <- DEU[DEU$CNTR_CODE=="DE","NUTS_NAME"]
#' terra::writeVector(DEU, "inst/extdata/DEU.gpkg", overwrite=TRUE)
#' 
#' EUR <- geodata::world(path=locdir())
#' EUR <- terra::crop(EUR, c(-11,25, 40,60))
#' terra::writeVector(EUR, "inst/extdata/EUR.gpkg", overwrite=TRUE)
#' ```
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2019, June 2023
#' @seealso [`plotRadar`], [website raster chapter](https://bookdown.org/brry/rdwd/raster-data.html)
#' @keywords aplot
#' @export
#' @examples
#' if(requireNamespace("terra", quietly=TRUE)){
#' plot(1, xlim=c(2,16), ylim=c(47,55))
#' addBorders()
#' addBorders(add=FALSE)
#' plot(1, xlim=c(2,16), ylim=c(47,55))
#' addBorders(de="orange", eu=NA)
#' }
#' 
#' @param de      Color for Bundeslaender line ([`DEU`]). NA to suppress. DEFAULT: "grey80"
#' @param eu      Color for countries line ([`EUR`]). NA to suppress. DEFAULT: "black"
#' @param add     Logical: add to existing plot? DEFAULT: TRUE
#' @param \dots   Further arguments passed to [terra::plot()]
addBorders <- function(
 de="grey80",
 eu="black",
 add=TRUE,
 ...)
{
checkSuggestedPackage("terra", "addBorders")
DEU <- terra::vect(system.file("extdata/DEU.gpkg", package="rdwd"))
EUR <- terra::vect(system.file("extdata/EUR.gpkg", package="rdwd"))
terra::plot(DEU, add=add, border=de, ...)
terra::plot(EUR, add=TRUE, border=eu, ...)
return(invisible(list(DEU=DEU, EUR=EUR)))
}



