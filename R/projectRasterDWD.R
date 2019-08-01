#' @title project DWD raster data
#' @description Set projection and extent for DWD raster data. Optionally (and
#' per default) also reprojects to latlon data.
#' The internal defaults are extracted from the
#' Kompositformatbeschreibung at \url{https://www.dwd.de/DE/leistungen/radolan/radolan.html},
#' as provided 2019-04 by Antonia Hengst.
#' @return Raster object with projection and extent, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2019
#' @seealso \code{raster::\link[raster]{crs}}, 
#'          \code{raster::\link[raster]{projection}},
#'          \code{raster::\link[raster]{extent}},
#'          \code{raster::\link[raster]{projectRaster}},
#'          \code{\link{readDWD.binary}, \link{readDWD.raster}, \link{readDWD.asc}}
#' @keywords aplot
#' @export
#' @examples
#' # To be used after readDWD.binary, readDWD.raster, readDWD.asc
#' @param r        Raster object
#' @param proj     Desired projection. 
#'                 Use NULL to not set proj+extent but still consider \code{latlon}.
#'                 Can be a \code{raster::\link[raster]{crs}} output,
#'                 a projection character string (will be passed to \code{crs}),
#'                 or a special charstring for internal defaults, namely:
#'                 "radolan" (see readDWD.binary + .asc) or "seasonal" (.raster).
#'                 DEFAULT: "radolan"
#' @param extent   Desired \code{\link[raster]{extent}}. Can be an extent object,
#'                 a vector with 4 numbers, or "radolan" / "rw" / "seasonal" 
#'                 with internal defaults.
#'                 DEFAULT: "radolan"
#' @param latlon   Logical: reproject \code{r} to lat-lon crs? DEFAULT: TRUE
#' @param quiet    Logical: suppress progress messages? DEFAULT: FALSE
#'
projectRasterDWD <- function(r, proj="radolan", extent="radolan", latlon=TRUE, quiet=FALSE)
{
# package check
checkSuggestedPackage("raster", "rdwd::projectRasterDWD")
#
starttime <- Sys.time()
if(!is.null(proj))
{
# Default projection and extent:
# Projection as per Kompositbeschreibung 1.5
p_radolan <- "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.93301270189
              +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs"
# ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/seasonal/air_temperature_max/
#       BESCHREIBUNG_gridsgermany_seasonal_air_temperature_max_de.pdf
# https://spatialreference.org/ref/epsg/31467/
p_seasonal <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 
               +ellps=bessel +datum=potsdam +units=m +no_defs"
#
if(is.character(proj))
  {   
  if(proj=="radolan")  proj <- p_radolan else
  if(proj=="seasonal") proj <- p_seasonal
  }
if(!inherits(proj, "CRS")) proj <- raster::crs(proj)
#
# Extent as per Kompositbeschreibung 1.4 / seasonal DESCRIPTION pdf:
e_radolan <- c(-523.4622,376.5378,-4658.645,-3758.645)
e_rw <-      c(-443.4622,456.5378,-4758.645,-3658.645) # 1.2, Abb 3
# e_radolan <- c(-673.4656656,726.5343344,-5008.642536,-3508.642536) # ME
e_seasonal <- c(3280414.71163347, 3934414.71163347, 5237500.62890625, 6103500.62890625)
if(is.character(extent))
  {  
  if(extent=="radolan")  extent <- e_radolan else
  if(extent=="rw")       extent <- e_rw      else
  if(extent=="seasonal") extent <- e_seasonal
  }
if(!inherits(extent,"Extent")) extent <- raster::extent(extent)
#
# actually project:
if(!quiet) message("Setting raster projection to ", proj, " ...")
raster::projection(r) <- proj
if(!quiet) message("Setting raster extent to ", toString(sapply(extent, I)), " ...")
raster::extent(    r) <- extent
} # end if not null proj
#
# lat-lon projection:
proj_ll <- raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
if(latlon) 
 {
 if(!quiet) message("Reprojecting raster to lat-lon ...")
 r <- raster::projectRaster(r, crs=proj_ll)
 }
dt <- difftime(Sys.time(),starttime)
if(!quiet) message("projectRasterDWD took ", round(dt,1), " ", attr(dt, "units"))
# invisible output:
return(invisible(r))
}
