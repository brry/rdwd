#' @title plot radar products on a pretty map
#' @description Convenience function to plot radar products on a pretty map.
#' Creates a separate plot for each layer, a selection is possible.
#' @return terra object, (re)projected (if `project=TRUE`).
#' If `length(layer)==1`, only that selected layer is returned.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2020, June 2023
#' @seealso [projectRasterDWD()], [addBorders()], [readDWD()],
#'          [website raster chapter](https://bookdown.org/brry/rdwd/raster-data.html)
#' @keywords aplot spatial
#' @importFrom graphics box rect par title
#' @importFrom berryFunctions seqPal tstop
#' @importFrom pbapply pblapply
#' @export
#' @examples
#' # See https://bookdown.org/brry/rdwd/raster-data.html
#' \dontrun{ ## Excluded from CRAN checks: requires internet connection
#' link <- "seasonal/air_temperature_mean/16_DJF/grids_germany_seasonal_air_temp_mean_188216.asc.gz"
#' rad <- dataDWD(link, base=gridbase, joinbf=TRUE)
#' radp <- plotRadar(rad, proj="seasonal", extent=NULL, main="plotRadar ex")
#' plotRadar(radp, ylim=c(52,54), project=FALSE)
#' 
#' # plotRadar equivalent, map only country borders:
#' radpm <- projectRasterDWD(rad[[1]], proj="seasonal", extent=NULL)
#' terra::plot(radpm)
#' addBorders()
#' 
#' # several layers
#' url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
#' nc <- dataDWD(url, base=gridbase, joinbf=TRUE)
#' 
#' ncp3 <- plotRadar(nc, main=paste(terra::longnames(nc), terra::time(nc)), layer=1:3,
#'                   col=terrain.colors(100), proj="nc", extent="nc")
#' plotRadar(ncp3, layer=3:4, project=FALSE) # still has all layers
#' plotRadar(ncp3, layer=4:5, project=FALSE, zlim="ind") # individual zlims per layer
#' plotRadar(ncp3, layer=1, project=FALSE, zlim=c(1016,1020))
#' 
#' ncp1 <- plotRadar(nc, layer=1, proj="nc", extent="nc") # much faster projection
#' # no longer has layers 2-4:
#' berryFunctions::is.error(plotRadar(ncp1, layer=1:4, project=FALSE), TRUE, TRUE)
#' }
#' 
#' @param x          terra raster oject, e.g. 'dat' element of object returned by [readDWD()].
#' @param layer      Optional: selected layer(s) to be plotted. DEFAULT: NULL
#' @param main       Graph title(s). Use "" to suppress.
#'                   DEFAULT: names(x)
#' @param land       Color of land areas in the map. DEFAULT: "gray80"
#' @param sea        Color of sea areas in the map. DEFAULT: "cadetblue1"
#' @param de         Color of Deutschland Bundesland borders ([`DEU`]). DEFAULT: "grey80"
#' @param eu         Color of Europe country borders ([`EUR`]). DEFAULT: "black"
#' @param col        Color palette for the data itself.
#'                   DEFAULT: [berryFunctions::seqPal()]
#' @param xlim       xlim. DEFAULT: NULL, i.e. taken from x extent (after reprojection if `project=TRUE`)
#' @param ylim       ylim. DEFAULT: NULL, i.e. taken from y extent (after reprojection if `project=TRUE`)
#' @param zlim       zlim. 3 Options: two-number vector,
#'                   `zlim="ind"` for individual zlim per layer,
#'                   or NULL for `range` of selected layer(s).
#'                   DEFAULT: NULL
#' @param axes       Draw axes? DEFAULT: TRUE
#' @param las        LabelAxisStyle for axes. DEFAULT: 1 (all upright)
#' @param mar        Vector with plot margins. DEFAULT: c(2.5, 3.5, 2.5, 5)
#' @param keeppar    Logical: keep the margins set with par, so later points etc are
#'                   added in the right location?
#'                   DEFAULT: TRUE, opposite to `sf::plot` with reset=TRUE, see
#'                   <https://github.com/cran/sf/blob/master/R/plot.R>
#' @param project    Project the data before plotting? Not needed if
#'                   [projectRasterDWD()] has already been called. DEFAULT: TRUE
#' @param proj       current projection, see [projectRasterDWD()],
#'                   used only if `project=TRUE`. DEFAULT: "radolan"
#' @param extent     current extent, see [projectRasterDWD()],
#'                   used only if `project=TRUE`. DEFAULT: "radolan"
#' @param adjust05   Logical: Adjust extent by 0.5m to match edges? DEFAULT: FALSE
#' @param targetproj target projection, see [projectRasterDWD()],
#'                   used only if `project=TRUE`. DEFAULT: "ll"
#' @param quiet      suppress progress messages? DEFAULT: FALSE through [rdwdquiet()]
#' @param \dots      Further arguments passed to [terra::plot()]
#' 
plotRadar <- function(
x,
layer=NULL,
main=names(x),
land="gray80",
sea="cadetblue1",
de="grey80",
eu="black",
col=berryFunctions::seqPal(),
xlim=NULL,
ylim=NULL,
zlim=NULL,
axes=TRUE,
las=1,
mar=c(2.5, 3.5, 2.5, 5),
keeppar=TRUE,
project=TRUE,
proj="radolan",
extent="radolan",
adjust05=FALSE,
targetproj="ll",
quiet=rdwdquiet(),
...
)
{
# Input checks:
checkSuggestedPackage("terra", "plotRadar")
if(identical(names(x),c("dat","meta"))) tstop("plotRadar needs the 'dat' element as input.")

force(main)
# projection (save time if layer is a single value):
if(length(layer)==1) x <- x[[layer]] # use only selected layer

if(project)
 {
 if(!quiet) message("- projecting:")
 x <- projectRasterDWD(x, proj=proj,extent=extent,adjust05=adjust05,targetproj=targetproj,quiet=quiet)
 }

if(!quiet) message("- preparing plots...")
# DEU / EUR borders:
DEU <- terra::vect(system.file("extdata/DEU.gpkg", package="rdwd"))
EUR <- terra::vect(system.file("extdata/EUR.gpkg", package="rdwd"))

singlemap <- function(x_i, main_i)
 {
 terra::plot(EUR, border=eu, col=land, xlim=xlim, ylim=ylim, background=sea, las=las)
 terra::plot(x_i, add=TRUE, range=zlim, col=col, ...)
 terra::plot(DEU, add=TRUE, border=de)
 title(main=main_i)
 }

lay <- 1:dim(x)[3]
lay2 <- lay
main <- rep(main, length.out=length(lay)) # recycle for all existing layers
if(length(layer)>1) lay <- lay[layer] # already done if layer length == 1
nn <- sum(!lay %in% lay2) # number not existing layers:
if(nn>0) tstop(nn, " layer",if(nn>1)"s", " selected that do",if(nn==1)"es"," not exist.")

# Extent:
ext <- terra::ext(x)
if(is.null(xlim)) xlim <- ext[1:2]
if(is.null(ylim)) ylim <- ext[3:4]
# zlim:
if(is.null(zlim)) 
 {
 zlim <- suppressWarnings(terra::minmax(x))
 if(any(!is.finite(zlim))) zlim <- t(terra::global(x, "range", na.rm=TRUE))
 }
if(is.matrix(zlim)) zlim <- range(zlim[,lay], na.rm=TRUE)
if(identical(zlim, "ind")) zlim <- NULL

# set graphical parameters:
op <- par(mar=mar)
if(!keeppar) on.exit(par(op), add=TRUE)

# Use function for each layer separately
if(!quiet) message("- plotting ", length(lay), " layer", if(length(lay)>1)"s", ":")
if(!quiet) lapply <- pbapply::pblapply
dummy <- lapply(lay, function(i) singlemap(x[[i]], main[i]) )

# Output:
return(invisible(x))
}
