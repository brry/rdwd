#' Handle Climate Data from DWD (German Weather Service)
#' 
#' - find, select, download + read data from the German weather service DWD\cr
#' - vectorized, progress bars, no re-downloads\cr
#' - index of files + meta data\cr
#' - observational time series from 6k meteorological recording stations (2.5k active)\cr
#'   -> rain, temperature, wind, sunshine, pressure, cloudiness, humidity, snow, ...\cr
#' - gridded raster data from radar + interpolation\cr
#' - european data stock slowly growing\cr
#' For an introduction to the package, see <https://bookdown.org/brry/rdwd>.\cr
#' 
#' @name rdwd
#' @aliases rdwd-package rdwd
#' @docType package
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}
#' @keywords package documentation
#' @seealso USA data: [countyweather](https://www.rdocumentation.org/packages/countyweather),
#'          [rnoaa](https://docs.ropensci.org/rnoaa/)\cr
#'          World data: [Global Surface Summary of the Day](https://ropensci.org/blog/blog/2017/04/04/gsodr)\cr
#'          Dutch data (Netherlands): <https://github.com/bvhest/KNMIr>\cr
#'          Canadian data: <https://cran.r-project.org/package=weathercan>\cr
#'          UK data website <https://www.metoffice.gov.uk/climate/uk/data>\cr
#' @section Searchability Terms:
#' Weather Data Germany download with R, Climate Data Germany\cr
#' Deutscher Wetterdienst R Daten download Klimastationen\cr
#' DWD Daten mit R runterladen, Wetter und Klimadaten in R
NULL
