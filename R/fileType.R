#' @title determine DWD file type
#' @description determine which subfunction to call in [readDWD()] from the file extension (ext).\cr\cr
#' The first block is for **observational data** ([overview](https://bookdown.org/brry/rdwd/available-datasets.html)),
#' the second for  **gridded data** ([overview](https://bookdown.org/brry/rdwd/raster-data.html)).\cr
#' Click on the `type` for the subfunction documentation, e.g. [data][readDWD.data] for [readDWD.data()].
#' 
#'  
#' | **type**               | **ext**     | **notes** |
#' |---                     | ---         | ---       | 
#' |                        |             |
#' |  [data][readDWD.data]  | .zip        | For regular data at [`dwdbase`].
#' |                        |             |
#' |  [meta][readDWD.meta]  | .txt        | For Beschreibung.txt files. For zip files containing station meta information, see [readMeta()].
#' |                        |             | 
#' |[multia][readDWD.multia]| \[SO\]      | \[SO\]: `file` ends with "Standort.txt". Overrides `meta`.
#' |                        |             |
#' | [stand][readDWD.stand] | \[SF\]      | \[SF\]: `file` contains "standard_format". For subdaily/standard_format files.
#' |                        |             |
#' |  -------               |             |
#' |                        |             |
#' | [radar][readDWD.radar] | .gz         | For when the `file` contains a single binary file.
#' |                        |             |
#' |[binary][readDWD.binary]| .tar.gz     | The common radolan format, as far as I can tell.
#' |                        |             |
#' |[raster][readDWD.raster]| .asc.gz     | E.g. for seasonal data at [`gridbase`].
#' |                        |             |
#' |    [nc][readDWD.nc]    | .nc.gz      | For netcdf files.
#' |                        |             |
#' |   [asc][readDWD.asc]   | .tar        | For a `file` containing asc files.
#'               
#' @return Character (vector)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2020
#' @seealso [readDWD()]
#' @keywords file
#' @importFrom berryFunctions truncMessage
#' @export
#' @examples
#' ft <- read.table(header=TRUE, stringsAsFactors=FALSE, text="
#' type    filename
#' data    daily_kl_recent_tageswerte_KL_03987_akt.zip
#' stand   subdaily_standard_format_kl_10381_00_akt.txt
#' meta    daily_kl_recent_KL_Tageswerte_Beschreibung_Stationen.txt
#' multia  multi_annual_mean_81-10_Temperatur_1981-2010_aktStandort.txt
#' 
#' binary  daily_radolan_historical_bin_2017_SF201712.tar.gz
#' raster  16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' nc      daily_Project_TRY_humidity_RH_199509_daymean.nc.gz
#' radar   radolan_recent_bin_raa01-rw_10000-1802020250-dwd---bin.gz
#' asc     radolan_historical_asc_2018_RW-201809.tar
#' ")
#' fileType(ft$filename)
#' 
#' stopifnot(fileType(ft$filename)==ft$type)
#' fileType("random_stuff.odt")
#'
#' @param file Filename(s) with extension.
#'
fileType <- function(file)
{
out <- rep("", length(file))
out[grepl(          ".zip$", file)] <- "data"
out[grepl(          ".txt$", file)] <- "meta"
out[grepl(  "Standort.txt$", file)] <- "multia"
out[grepl("standard_format", file)] <- "stand"

out[grepl(           ".gz$", file)] <- "radar"
out[grepl(       ".tar.gz$", file)] <- "binary"
out[grepl(       ".asc.gz$", file)] <- "raster"
out[grepl(        ".nc.gz$", file)] <- "nc"
out[grepl(          ".tar$", file)] <- "asc"

if(any(out=="")) warning("fileType failed for the following file", 
                         truncMessage(file[out==""], midfix=": "))

return(out)
}
