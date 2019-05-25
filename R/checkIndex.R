#' @title check indexes
#' @description check indexes. Mainly for internal usage in \code{\link{createIndex}}.
#'              Not exported, so call it as rdwd:::checkIndex() if you want to
#'              run tests yourself. Further test suggestions are welcome!
#' @return named list with issues (if any)
#' @importFrom berryFunctions sortDF
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2019
#' @seealso \code{\link{createIndex}}
#' @examples 
#' data(fileIndex) ; data(metaIndex) ; data(geoIndex)
#' # ci <- checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex)
#' @param findex    \code{\link{fileIndex}}. DEFAULT: NULL
#' @param mindex    \code{\link{metaIndex}}. DEFAULT: NULL
#' @param gindex    \code{\link{geoIndex}}.  DEFAULT: NULL
#' @param excludefp Exclude false positives from geoIndex coordinate check results?
#'                  DEFAULT: TRUE
#' @param fast      Exclude the 3-minute location per ID check? DEFAULT: FALSE
checkIndex <- function(findex=NULL, mindex=NULL, gindex=NULL, excludefp=TRUE, fast=FALSE)
{
# helper function:
alldupli <- function(x) duplicated(x) | duplicated(x, fromLast=TRUE)
# Outputs:
out <- list(time=Sys.time())
wmes <- "" # composit warning message

# findex ----

if(!is.null(findex)){
message("Checking fileIndex...")
# check for duplicate files (DWD errors):
duplifile <- findex[!grepl("minute",findex$res),] # 1min + 10min excluded
duplifile <- duplifile[alldupli(duplifile[,1:4]),]
duplifile <- duplifile[!is.na(duplifile$id),] # exclude meta + multia files
# ignore only 2019-05-16:
duplifile <- duplifile[duplifile$var!="more_precip",]
if(nrow(duplifile)>0)
  {
  wmes <- paste0(wmes, "\n- There are ", length(unique(duplifile$id)), 
                 " stations with more than one file, see output duplifile")
  out$duplifile <- duplifile
  }
}


# mindex ----

if(!is.null(mindex)){
message("Checking metaIndex...")
# helper functions:
newmes <- function(log, text) paste0(wmes, "\n- There are ", sum(log), " stations with ", 
                                     text, ", see output ", deparse(substitute(log)))
newout <- function(ids,colcomp,column,textvar,unit="") sapply(ids, function(i) 
    {tt <- sort(table(mindex[colcomp==i,column]), decreasing=TRUE)
     unname(paste0(textvar,"=",i, ": ", paste0(tt,"x",names(tt),unit, collapse=", ")))
    })
id_uni <- unique(mindex$Stations_id)

# ID elevation inconsistencies:
eletol <- 2.1 # m tolerance
id_ele <- pbapply::pbsapply(id_uni, function(i) 
                 any(abs(diff(mindex[mindex$Stations_id==i,"Stationshoehe"]))>eletol))
if(any(id_ele))
  {
  wmes <- newmes(id_ele, "elevation inconsistencies across Beschreibung files")
  out$id_elev <- newout(id_uni[id_ele], mindex$Stations_id, "Stationshoehe", "ID", "m")
  }

# several locations for one station ID:
if(!fast){
loctol <- 0.050 # km
id_loc <- pbapply::pbsapply(id_uni, function(i) 
    maxlldist("geoBreite","geoLaenge", mindex[mindex$Stations_id==i,], each=FALSE)>loctol)
mindex$coord <- paste(mindex$geoBreite, mindex$geoLaenge, sep="_")
if(any(id_loc))
  {
  wmes <- newmes(id_loc, "more than one set of coordinates")
  out$id_loc <- newout(id_uni[id_loc], mindex$Stations_id, "coord", "ID") 
  }
}

# Different names per ID:
id_name <- pbapply::pbsapply(id_uni, function(i) 
                  length(unique(mindex[mindex$Stations_id==i,"Stationsname"]))>1)
if(any(id_name))
  {
  wmes <- newmes(id_name, "more than one name per id")
  out$id_name <- newout(id_uni[id_name], mindex$Stations_id, "Stationsname", "ID")
  }

# Different IDs per name:
name_uni <- unique(mindex$Stationsname)
name_id <- pbapply::pbsapply(name_uni, function(n) 
                  length(unique(mindex[mindex$Stationsname==n,"Stations_id"]))>1)
if(any(name_id))
  {
  wmes <- newmes(name_id, "more than one id per name")
  out$name_id <- newout(name_uni[name_id], mindex$Stationsname,"Stations_id", "Name")
  }
}


# gindex ----

if(!is.null(gindex)){
message("Checking geoIndex...")
columns <- !colnames(gindex) %in% c("display","col")
# Duplicate coordinates checks:
# Exclude known false positives like "Dasburg" vs "Dasburg (WWV RLP)"
fpid <- c(14306,921, 13967,13918, 14317,3024, 2158,7434, 785,787, 15526, 5248,5249, 396,397)
# sortDF(gindex[gindex$id %in% fpid, columns], "name")
gindex_id <- gindex
if(excludefp) gindex_id <- gindex[!gindex$id %in% fpid,]
coord <- paste(gindex_id$lon, gindex_id$lat, sep="_")
# several stations at the same locations:
dupli_id <- alldupli(coord)
if(any(dupli_id))
  {
  wmes <- paste0(wmes, "\n- There are ", sum(dupli_id)/2, " sets of coordinates ",
                 "used for more than one station id, see output ids_at_one_loc")
  out$ids_at_one_loc <- sortDF(gindex_id[dupli_id, columns], "lon", decreasing=FALSE)
  rownames(out$ids_at_one_loc) <- NULL
  }
}

# output stuff:
if(wmes!="") warning(wmes)
return(invisible(out))
} # end checkIndex
