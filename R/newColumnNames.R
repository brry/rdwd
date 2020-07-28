#' @title Enhance readDWD column names
#' @description Add short German parameter descriptions to the DWD abbreviations.
#' This uses [dwdparams()] to create column names like
#' "TT_TU.Lufttemperatur" and "RSK.Niederschlagshoehe."
#' Column names not in the abbreviation list will be left untouched.
#' @return The `dataframe` with new column names
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Apr 2019
#' @seealso [`dwdparams`], [readVars()],  [readDWD()] argument `varnames`, [newColumnNames()]
#' @export
#' @examples
#' # mainly for internal usage
#' 
#' @param dataframe Dataframe as returned by [readDWD.data()]
#' @param variables Dataframe as returned by [readVars()] for a
#'                  single file. Rownames must be variable abbreviations.
#'                  There must be a "Kurz" column.
#'                  DEFAULT: [`dwdparams`]
#' @param separator Separator between abbreviation and long name. DEFAULT: "."
#' 
newColumnNames <- function(
dataframe,
variables=dwdparams,
separator="."
)
{
nn <- variables[colnames(dataframe), "Kurz"] # nn: new names
notna <- !is.na(nn)
nn <- paste0(colnames(dataframe), separator, nn)
colnames(dataframe)[notna] <- nn[notna]
return(dataframe)
}
