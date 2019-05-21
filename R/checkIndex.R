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
#' # rdwd:::checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex)
#' @param findex    \code{\link{fileIndex}}. DEFAULT: NULL
#' @param mindex    \code{\link{metaIndex}}. DEFAULT: NULL
#' @param gindex    \code{\link{geoIndex}}.  DEFAULT: NULL
#' @param excludefp Exclude false positives from geoIndex coordinate check results?
#'                  DEFAULT: TRUE
#'
checkIndex <- function(findex=NULL, mindex=NULL, gindex=NULL, excludefp=TRUE)
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
# ID elevation inconsistencies:
eletol <- 2.1 # m tolerance
id_uni <- unique(mindex$Stations_id)
id_ele <- pbapply::pbsapply(id_uni, function(i) 
                 any(abs(diff(mindex[mindex$Stations_id==i,"Stationshoehe"]))>eletol))
if(any(id_ele))
  {
  wmes <- paste0(wmes, "\n- There are ", sum(id_ele), " stations with elevation ",
                 "inconsistencies across Beschreibung files, see outputs ",
                 "id_elev and id_elev_summary")
  out$id_elev <- mindex[mindex$Stations_id %in% id_uni[id_ele], ]
  rownames(out$id_elev) <- NULL
  out$id_elev_summary <- sapply(id_uni[id_ele], function(i) 
    {tt <- sort(table(mindex[mindex$Stations_id==i,"Stationshoehe"]), decreasing=TRUE)
     paste0("ID=",i, ": ", paste0(tt,"x",names(tt),"m", collapse=", "))
    })
  }

# several locations for one station ID:
loctol <- 0.050 # km
id_loc <- pbapply::pbsapply(id_uni, function(i) 
    maxlldist("geoBreite","geoLaenge", mindex[mindex$Stations_id==i,], each=FALSE)>loctol)
if(any(id_loc))
  {
  wmes <- paste0(wmes, "\n- There are ", sum(id_loc), " stations with more ",
                 "than one set of coordinates, see outputs locs_at_one_id and id_locs_summary")
  out$locs_at_one_id <- mindex[mindex$Stations_id %in% id_uni[id_loc], ]
  rownames(out$locs_at_one_id) <- NULL
  coord <- paste(mindex$geoBreite, mindex$geoLaenge, sep="_")
  out$id_loc_summary <- sapply(id_uni[id_loc], function(i) 
    {tt <- sort(table(coord[mindex$Stations_id==i]), decreasing=TRUE)
     paste0("ID=",i, ": ", paste0(tt,"x",names(tt), collapse=", "))
    })
  }

# Different names per ID:
id_name <- pbapply::pbsapply(id_uni, function(i) 
                  length(unique(mindex[mindex$Stations_id==i,"Stationsname"]))>1)
if(any(id_name))
  {
  wmes <- paste0(wmes, "\n- There are ", sum(id_name), 
                 " stations with more than one name per id, see output id_name")
  out$id_name <- sapply(id_uni[id_name], function(i) 
    {tt <- sort(table(mindex[mindex$Stations_id==i,"Stationsname"]), decreasing=TRUE)
     paste0("ID=",i, ": ", paste0(tt,"x",names(tt), collapse=", "))
    })
  }
# Different IDs per name:
name_uni <- unique(mindex$Stationsname)
name_id <- pbapply::pbsapply(name_uni, function(n) 
                  length(unique(mindex[mindex$Stationsname==n,"Stations_id"]))>1)
if(any(name_id))
  {
  wmes <- paste0(wmes, "\n- There are ", sum(name_id), 
                 " stations with more than one id per name, see output id_per_name")
  out$id_per_name <- unname(sapply(name_uni[name_id], function(n) 
    {tt <- sort(table(mindex[mindex$Stationsname==n,"Stations_id"]), decreasing=TRUE)
     paste0("Name=",n, ": ", paste0(tt,"x",names(tt), collapse=", "))
    }))
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
