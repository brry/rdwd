#' @title plot radar products on a pretty map
#' @description Convenience function to plot radar products on a pretty map. 
#' Creates a separate plot for each layer, a selection is possible.
#' @return raster object, projected (if \code{project=TRUE}).
#' If \code{length(layer)==1}, only that selected layer is returned.
#' If \code{main} is non-empty, it is added to \code{x@@title}.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Feb 2020
#' @seealso \code{\link{readDWD}}, \url{https://bookdown.org/brry/rdwd/raster-data.html}
#' @keywords aplot spatial
#' @importFrom graphics box rect
#' @export
#' @examples
#' # See homepage in the section 'See Also'
#' \dontrun{ ## Excluded from CRAN checks: requires internet connection
#' link <- "seasonal/air_temperature_mean/16_DJF/grids_germany_seasonal_air_temp_mean_188216.asc.gz"
#' rad <- dataDWD(link, base=gridbase, joinbf=TRUE, dir=tempdir())
#' plotRadar(rad, proj="seasonal", extent=rad@extent)
#' plotRadar(rad, ylim=c(52,54), proj="seasonal", extent=rad@extent)
#' plotRadar(rad)
#' }
#'
#' @param x          raster oject, e.g. 'dat' element of object returned by \code{\link{readDWD}}.
#' @param layer      Optional: selected layer(s) to be plotted. DEFAULT: NULL
#' @param main       Graph title(s). DEFAULT: ""
#' @param land       Color of land areas in the map. DEFAULT: "gray80"
#' @param sea        Color of sea areas in the map. DEFAULT: "cadetblue1"
#' @param de         Color of Deutschland Bundesland borders. DEFAULT: "grey80"
#' @param eu         Color of Europe country borders. DEFAULT: "black"
#' @param xlim       xlim. DEFAULT: NULL, i.e. taken from x extent (after reprojection if \code{project=TRUE})
#' @param ylim       ylim. DEFAULT: NULL, i.e. taken from y extent (after reprojection if \code{project=TRUE})
#' @param project    Project the data before plotting? Not needed if 
#'                   \code{\link{projectRasterDWD}} has already been called. DEFAULT: TRUE
#' @param proj       current projection, see \code{\link{projectRasterDWD}}, 
#'                   used only if \code{project=TRUE}. DEFAULT: "radolan"
#' @param extent     current extent, see \code{\link{projectRasterDWD}}, 
#'                   used only if \code{project=TRUE}. DEFAULT: "radolan"
#' @param targetproj target projection, see \code{\link{projectRasterDWD}}, 
#'                   used only if \code{project=TRUE}. DEFAULT: "ll"
#' @param quiet      suppress progress messages? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
#' @param \dots      Further arguments passed to \code{raster::\link[raster]{plot}}
#'
plotRadar <- function(
x,
layer=NULL,
main="",
land="gray80",
sea="cadetblue1",
de="grey80", 
eu="black",
xlim=NULL,
ylim=NULL,
project=TRUE,
proj="radolan",
extent="radolan",
targetproj="ll",
quiet=rdwdquiet(),
...
)
{
# Input checks:
checkSuggestedPackage("raster", "plotRadar") 
if(identical(names(x),c("dat","meta"))) stop("plotRadar needs the 'dat' element as input.")

# projection (save time if layer is a single value):
if(length(layer)==1) x <- x[[layer]] # use only selected layer
if(project) 
 {
 if(!quiet) message("- projecting:")
 x <- projectRasterDWD(x, proj=proj,extent=extent,targetproj=targetproj,quiet=quiet)
 }
# Extent:
ext <- raster::extent(x)
if(is.null(xlim)) xlim <- c(ext@xmin, ext@xmax)
if(is.null(ylim)) ylim <- c(ext@ymin, ext@ymax)
# main
if(!identical(main,"")) x@title <- c(x@title, main)

# DEU / EUR borders:
load(system.file("extdata/DEU.rda", package="rdwd"), envir=environment())
load(system.file("extdata/EUR.rda", package="rdwd"), envir=environment())

singlemap <- function(x_i, main_i)
  {
  # even if manually given, xlim/ylim not consistently adhered to for different x
  # hence plot is set up with DEU
  raster::plot(DEU, add=FALSE, xlim=xlim, ylim=ylim) 
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=sea)
  raster::plot(EUR, add=TRUE, col=land)
  box()
  raster::plot(x_i, add=TRUE, ...)
  raster::plot(DEU, add=TRUE, border=de)
  raster::plot(EUR, add=TRUE, border=eu)
  title(main=main_i)
  }

# Use function for each layer separately
lay <- 1:raster::nlayers(x)
if(length(layer)>1) lay <- lay[layer]
main <- rep(main, length.out=length(lay))
if(!quiet) lapply <- pbapply::pblapply
if(!quiet) message("- plotting ", length(lay), " layer", if(length(lay)>1)"s", ":")
dummy <- lapply(lay, function(i) singlemap(x[[i]], main[i]) )

# Output:
return(invisible(x))
}
