#' Information for a station ID on the DWD CDC FTP server
#' 
#' @return invisible data.frame. Also \code{\link{print}s} the output nicely formatted.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2016
#' @seealso [`metaIndex`]
#' @keywords datasets
#' @importFrom berryFunctions sortDF
#' @export
#' @examples
#' metaInfo(2849)
#' 
#' @param id Station ID (integer number or convertible to one)
#' @param mindex      Index dataframe with metadata. DEFAULT: [`metaIndex`]
#' @param hasfileonly Logical: Only show entries that have files? DEFAULT: TRUE
#' 
metaInfo <- function(
  id,
  mindex=metaIndex,
  hasfileonly=TRUE
  )
{
mindexname <- deparse(substitute(mindex))
# ID preparation:
id <- as.integer(id[1])
# Selection of rows:
sel <- mindex$Stations_id==id
if(sum(sel)<1) stop(mindexname," contains no entries for id=", id,
                    ". This ID probably does not exist.")
# public / nonpublic files
nonpubmes <- ""
nonpub <- !mindex[sel,"hasfile"]
if(any(nonpub)&hasfileonly) nonpubmes <- paste0("\nAdditionally, there are ",
      sum(nonpub), " non-public files. Display all with  metaInfo(",id,",FALSE)",
      "\nTo request those datasets, please contact cdc.daten@dwd.de or klima.vertrieb@dwd.de")
if(hasfileonly) sel <- sel & mindex$hasfile
# Output preparation:
out <- mindex[sel,]
#
# Print preparation I:
p_id <- toString(unique(out$Stations_id))
p_sn <- toString(unique(out$Stationsname))
p_bl <- toString(unique(out$Bundesland))
p_nf <- length(unique(paste(out$res, out$var, out$per)))
if(p_id=="") p_id <- id
# message I:
message("rdwd station id ", p_id, " with ", p_nf, " files.\n",
        "Name: ", p_sn, ", State: ", p_bl, 
        "\nFor up-to-date info, see https://bookdown.org/brry/rdwd/fileindex.html#metaindex",
        nonpubmes)
#
if(nrow(out)==0) return()
#
# Print preparation II:
p_out <- data.frame(from=out$von_datum,
                    to=out$bis_datum,
                    lat=out$geoBreite,
                    long=out$geoLaenge,
                    ele=out$Stationshoehe)
p_out <- cbind(out[,c("res","var","per","hasfile")], p_out)
p_out$from <- as.character(p_out$from)
###p_out$from[p_out$per=="recent"] <- ""
p_out <- sortDF(p_out, "var", decreasing=FALSE)
p_out <- sortDF(p_out, "res", decreasing=FALSE)
p_out <- sortDF(p_out, "per", decreasing=FALSE)
rownames(p_out) <- NULL
# print II:
print(p_out, quote=FALSE)
#
# Output:
return(invisible(out))
}
