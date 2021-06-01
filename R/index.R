# fileIndex, metaIndex, geoIndex, gridIndex, formatIndex ----

#' Indexes of files and metadata on the DWD CDC FTP server
#' 
#' Created with [indexFTP()] and [createIndex()] used in [updateIndexes()].\cr
#' In functions, you can access them with `rdwd:::fileIndex` etc.\cr
#' **fileIndex**: A data.frame with the filenames (and derived information)
#' at the default `base` value [`dwdbase`].\cr
#' **metaIndex**: A data.frame with the contents of all the station description files
#' (..._Beschreibung_Stationen.txt) under [`dwdbase`].\cr
#' **geoIndex**: `metaIndex` distilled to geographic locations.\cr
#' **gridIndex**: Vector of file paths at [`gridbase`].\cr
#' **formatIndex**: (modified) table from
#' <https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/subdaily/standard_format/formate_kl.html>\cr
#' 
#' @name index
#' @aliases fileIndex metaIndex geoIndex gridIndex formatIndex
#' @docType data
#' @format
#' **fileIndex**: data.frame with character strings. ca 260k rows x 8 columns:\cr
#'         `res`, `var`, `per` (see [selectDWD()]),
#'         station `id`, time series `start` and `end`, and
#'         `ismeta` information, all according to `path`.\cr
#' **metaIndex**: data.frame with ca 97k rows for 12 columns:\cr
#'         `Stations_id, von_datum, bis_datum,
#'         Stationshoehe, geoBreite, geoLaenge, Stationsname, Bundesland,
#'         res, var, per, hasfile` \cr
#' **geoIndex**: data.frame with ca 6k rows for 11 columns:\cr
#'         `id, name, state, lat, lon, ele, nfiles, nonpublic, recentfile, display, col`\cr
#' **gridIndex**: Vector with ca 50k file paths at [`gridbase`]\cr
#' **formatIndex**: data.frame with 140 rows for 12 columns:\cr
#'         `Ke_Ind, Kennung, Label, Beschreibung, Einheit, Code-Tabellen,
#'         Zusatzinfo, Typ, Pos, Erlaubt, Fehlk, dividebyten`\cr
#' @source Deutscher WetterDienst / Climate Data Center  FTP Server
#' @seealso [createIndex()], [indexFTP()], [selectDWD()],
#'          [findID()], [metaInfo()],
#'          [website index chapter](https://bookdown.org/brry/rdwd/fileindex.html)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, June-Nov 2016, June 2017, Oct 2019
#' @keywords datasets
#' @importFrom utils data
#' @examples
#' data(fileIndex)
#' data(metaIndex)
#' data(geoIndex)
#' head(fileIndex)
#' head(metaIndex)
#' head(geoIndex)
#' 
#' # in functions, you can use head(rdwd:::fileIndex) etc, but I don't export them
#' # because Hadley says 'Never @export a data set' in
#' # browseURL("http://r-pkgs.had.co.nz/data.html#data-data")
#' 
#' \dontrun{ # Excluded from CRAN checks to avoid file creation
#' 
#' # To use a custom index, since especially gridfiles names are updated daily:
#' # library(rdwd)
#' customFolders <- c("monthly/air_temperature_mean","daily/Project_TRY/pressure")
#' customFiles <- indexFTP(customFolders, base=gridbase, dir=tempdir())
#' customIndex<- createIndex(customFiles, dir=tempdir())
#' browseURL("https://bookdown.org/brry/rdwd/fileindex.html")
#' }
#' 
data(fileIndex, envir=environment())
data(metaIndex, envir=environment())
data( geoIndex, envir=environment())
data(gridIndex, envir=environment())
data(formatIndex, envir=environment())
# http://stackoverflow.com/questions/32964741/accessing-sysdata-rda-within-package-functions
# http://stackoverflow.com/questions/9521009/how-do-you-handle-r-data-internal-to-a-package
