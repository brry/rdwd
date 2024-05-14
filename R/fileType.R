
#' @title valid fileType values
#' @description fileType values that have a reading subfunction `readDWD.ftype()`.
#' @export
validFileTypes <- strsplit("data,meta,multia,stand,deriv,radar,binary,raster,nc,hyras,asc,rklim,grib2,pdf", ",")[[1]]



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
#' |[multia][readDWD.multia]| \[SO\]      | \[SO\]: `file` ends with "Standort.txt" or contains multi_annual. Overrides `meta`.
#' |                        |             |
#' | [stand][readDWD.stand] | \[SF\]      | \[SF\]: `file` contains "standard_format". For subdaily/standard_format files.
#' |                        |             |
#' |  [data][readDWD.deriv] | .txt.gz     | For data at /CDC/derived_germany/.
#' |                        |             |
#' |   [pdf][readDWD.pdf]   | .pdf        | only opens `file` in default viewer.
#' |                        |             |
#' |  -------               |             |
#' |                        |             |
#' | [radar][readDWD.radar] | .gz         | For when the `file` contains a single binary file.
#' |                        |             |
#' |[binary][readDWD.binary]| .tar.gz     | The common radolan format, as far as I can tell.
#' |                        |             |
#' |[raster][readDWD.raster]| .asc.gz     | E.g. for seasonal data at [`gridbase`].
#' |                        |             |
#' |    [nc][readDWD.nc]    | .nc.gz      | For packed netcdf files.
#' |                        |             |
#' | [hyras][readDWD.hyras] | .nc         | For non-packed netcdf files.
#' |                        |             |
#' |   [asc][readDWD.asc]   | .tar        | For a `file` containing asc files.
#' |                        |             |
#' | [rklim][readDWD.rklim] | YW*.tar     | For a `file` containing bin files.
#' |                        |             |
#' | [grib2][readDWD.grib2] | .grib2.bz2  | For an nwp forecast `file`.
#'               
#' @return Character (vector)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2020
#' @seealso [readDWD()]
#' @keywords file
#' @importFrom berryFunctions truncMessage twarning
#' @export
#' @examples
#' ft <- read.table(header=TRUE, stringsAsFactors=FALSE, text="
#' type    filename
#' data    daily_kl_recent_tageswerte_KL_03987_akt.zip
#' meta    daily_kl_recent_KL_Tageswerte_Beschreibung_Stationen.txt
#' multia  multi_annual_mean_81-10_Temperatur_1981-2010_aktStandort.txt
#' multia  multi_annual_mean_81-10_Temperatur_1981-2010.txt
#' stand   subdaily_standard_format_kl_10381_00_akt.txt
#' deriv   derived_germany_soil_daily_historical_3987.txt.gz
#' pdf     DESCRIPTION_obsgermany_climate_monthly_kl_historical_en.pdf
#' 
#' radar   radolan_recent_bin_raa01-rw_10000-1802020250-dwd---bin.gz
#' binary  daily_radolan_historical_bin_2017_SF201712.tar.gz
#' raster  16_DJF_grids_germany_seasonal_air_temp_mean_188216.asc.gz
#' nc      daily_Project_TRY_humidity_RH_199509_daymean.nc.gz
#' hyras   monthly_hyras_de_humidity_hurs_hyras_5_2020_v5-0_de_monmean.nc
#' asc     radolan_historical_asc_2018_RW-201809.tar
#' rklim   5_minutes_radolan_reproc_2017_002_bin_2020_YW2017.002_202006.tar
#' grib2   ftp_weather_nwp_cosmo-d2_005_T_2M.grib2.bz2
#' ")
#' fileType(ft$filename)
#' 
#' stopifnot(fileType(ft$filename)==ft$type)
#' berryFunctions::is.error(fileType("random_stuff.odt"), force=TRUE)
#' 
#' stopifnot(validFileTypes %in% ft$type)
#' stopifnot(ft$type %in% validFileTypes)
#'
#' @param file Filename(s) with extension.
#'
fileType <- function(file)
{
out <- rep("fileTypeError", length(file))
out[grepl(          ".zip$", file)] <- "data"
out[grepl(          ".txt$", file)] <- "meta"
out[grepl(  "Standort.txt$", file)] <- "multia"
out[grepl(  "multi.?annual", file)] <- "multia" # new format 2022
out[grepl("standard_format", file)] <- "stand"
out[grepl("Standardformate_Beschreibung", file)] <- "meta" # 2024-05 for stand Stationenbeschreibung
out[grepl(          ".pdf$", file)] <- "pdf"

out[grepl(           ".gz$", file)] <- "radar"
out[grepl(       ".tar.gz$", file)] <- "binary"
out[grepl(       ".asc.gz$", file)] <- "raster"
out[grepl(        ".nc.gz$", file)] <- "nc"
out[grepl(           ".nc$", file)] <- "hyras"
out[grepl(          ".tar$", file)] <- "asc"
out[grepl(      "YW.*.tar$", file)] <- "rklim"
out[grepl(    ".grib2.bz2$", file)] <- "grib2"
out[grepl(       ".txt.gz$", file)] <- "deriv"

if(any(out=="fileTypeError")) tstop("fileType failed for the following file", 
             truncMessage(file[out=="fileTypeError"], midfix="\n- ", sep="\n- "))

return(out)
}
