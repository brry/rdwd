#' @title check indexes
#' @description check indexes. Mainly for internal usage in [createIndex()].
#'              Not exported, so call it as rdwd:::checkIndex() if you want to
#'              run tests yourself. Further test suggestions are welcome!
#' @return Charstring with issues (if any) to be printed with [message()].
#' @importFrom berryFunctions truncMessage round0 traceCall twarning
#' @importFrom pbapply pbsapply
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, May 2019
#' @seealso [`createIndex`]
#' @examples
#' data(fileIndex) ; data(metaIndex) ; data(geoIndex)
#' # ci <- rdwd:::checkIndex(findex=fileIndex, mindex=metaIndex, gindex=geoIndex)
#' # cat(ci)
#' @param findex    [`fileIndex`]. DEFAULT: NULL
#' @param mindex    [`metaIndex`]. DEFAULT: NULL
#' @param gindex    [`geoIndex`].  DEFAULT: NULL
#' @param excludefp Exclude false positives from geoIndex coordinate check results?
#'                  DEFAULT: TRUE
#' @param fast      Exclude the 3-minute location per ID check? DEFAULT: FALSE
#' @param warn      Warn about issues? DEFAULT: `!quiet` (TRUE)
#' @param logfile   File to copy log to, appended to existing content. NULL to suppress.
#'                  DEFAULT: "misc/ExampleTests/warnings.txt"
#' @param quiet     Logical: Suppress progress messages?
#'                  DEFAULT: FALSE through [rdwdquiet()]
checkIndex <- function(
  findex=NULL,
  mindex=NULL,
  gindex=NULL,
  excludefp=TRUE,
  fast=FALSE,
  warn=!quiet,
  logfile=berryFunctions::packagePath(file="misc/ExampleTests/warnings.txt"),
  quiet=rdwdquiet()
  )
{
# helper function:
alldupli <- function(x) duplicated(x) | duplicated(x, fromLast=TRUE)
# Output text:
out <- paste0("\ncheckIndex results at ", format(Sys.time(),"%F %T"), " for\n", dwdbase)
itime <- file.mtime("data/fileIndex.rda")
if(!is.na(itime)) out <- paste0(out, "\nFile 'data/fileIndex.rda' was last modified ", itime)
out <- paste0(out, berryFunctions::traceCall(skip=1), if(excludefp) "using excludefp=TRUE\n", "-------")

# findex ----

if(!is.null(findex)){
if(!quiet) message("Checking fileIndex...")
# check for duplicate files (DWD errors):
duplifile <- findex[!grepl("minute",findex$res, fixed=TRUE),] # 1min + 10min excluded
duplifile <- duplifile[!grepl("climate_indices/kl", duplifile$path, fixed=TRUE),]
duplifile <- duplifile[alldupli(duplifile[,1:4]),]
duplifile <- duplifile[!is.na(duplifile$id),] # exclude meta + multia files
duplifile <- duplifile[duplifile$res!="subdaily" & duplifile$var!="standard_format",]

if(nrow(duplifile)>0)
  {
  rvp <- dirname(duplifile$path)
  per_folder <- lapply(unique(rvp), function(p)
    {i <- unique(duplifile$id[rvp==p])
    paste0("- ", berryFunctions::round0(length(i), pre=2, flag=" "), " at ", p, "; ",
           berryFunctions::truncMessage(i, ntrunc=10, prefix=""))
    })
  per_folder <- paste(unlist(per_folder), collapse="\n")
  out <- c(out, "IDs with duplicate files:", per_folder)
  }

# Duplicate meta files:
duplifile <- findex[findex$ismeta &
                    grepl("txt$", findex$path) &
                    !grepl("minute",findex$res, fixed=TRUE) &
                    !grepl("FX3",findex$path, fixed=TRUE) &
                    findex$var != "climate_indices" &
                    findex$var != "weather_phenomena" &
                    findex$res != "multi_annual",] # aktStandort + festerStandort
duplifile$rvp <- paste(duplifile$res, duplifile$var, duplifile$per, sep="/")
duplifile <- duplifile$path[alldupli(duplifile$rvp)]
if(length(duplifile)>0)
  out <- c(out, "Duplicate 'Beschreibung' files:", paste("-",duplifile))
}


# mindex ----

if(!is.null(mindex)){
if(!quiet) message("Checking metaIndex...")
# helper function:
newout <- function(out,ids,colcomp,column,textvar,unit="")
 {
 new <- sapply(ids, function(i)
 {tt <- sort(table(mindex[colcomp==i,column]), decreasing=TRUE)
 unname(paste0("- ", textvar,"=",i, ": ", paste0(tt,"x",names(tt),unit, collapse=", ")))
 })
 c(out, new)
 }

id_uni <- unique(mindex$Stations_id)
# ID elevation inconsistencies:
eletol <- 4.1 # too many with old 2.1 # m tolerance
id_ele <- pbapply::pbsapply(id_uni, function(i)
                 any(abs(diff(mindex[mindex$Stations_id==i,"Stationshoehe"]))>eletol))
if(any(id_ele))
  {
  out <- c(out,paste0("\nElevation differences >",eletol,"m at ",sum(id_ele)," stations:"))
  out <- newout(out, id_uni[id_ele], mindex$Stations_id, "Stationshoehe", "ID", "m")
  }

# several locations for one station ID:
if(!fast){
loctol <- 0.040 # km
if(!quiet) message("expect 1:30 minutes in the next loop (turn off with fast=TRUE):")
id_loc <- pbapply::pbsapply(id_uni, function(i)
    maxlldist("geoBreite","geoLaenge", mindex[mindex$Stations_id==i,], each=FALSE)>loctol)
mindex$coord <- paste(mindex$geoBreite, mindex$geoLaenge, sep="_")
if(any(id_loc))
  {
  out <- c(out, paste0("\nLocation differences >",loctol*1000,"m at ",sum(id_loc)," stations:"))
  out <- newout(out, id_uni[id_loc], mindex$Stations_id, "coord", "ID")
  }
}

# Different names per ID:
id_name <- pbapply::pbsapply(id_uni, function(i)
                  length(unique(mindex[mindex$Stations_id==i,"Stationsname"]))>1)
# Exclude known false positives like 33xOsterburg (Altmark)-Ballerstedt, 1xBallerstedt
fpid <- c(7372, 2749, 5985, 7492, 3238, 7492, 3238, 7078, 4080, 1495, 1803, 7077, 
          1888, 15526, 4412, 4618, 2667, 15003, 3297, 3509, 3881, 4352)
if(excludefp) id_name[id_uni %in% fpid] <- FALSE
if(any(id_name))
  {
  out <- c(out, paste0("\nDifferent names at ",sum(id_name)," IDs:"))
  out <- newout(out, id_uni[id_name], mindex$Stations_id, "Stationsname", "ID")
  }

# Different IDs per name:
name_uni <- unique(mindex$Stationsname)
name_id <- pbapply::pbsapply(name_uni, function(n)
                  length(unique(mindex[mindex$Stationsname==n,"Stations_id"]))>1)
if(excludefp) name_id[name_uni=="Suderburg"] <- FALSE
if(any(name_id))
  {
  out <- c(out, paste0("\nMore than one id per name at ",sum(name_id)," names:"))
  out <- newout(out, name_uni[name_id], mindex$Stationsname,"Stations_id", "Name")
  }
}



# file/metaIndex date ranges  ----

# currently suppressed - too many differences to be meaningful!
if(!is.null(findex) & !is.null(mindex) & FALSE){
if(!quiet) message("Comparing fileIndex and metaIndex date ranges...")

findex$start <- as.Date(findex$start, "%Y%m%d")
findex$end   <- as.Date(findex$end,   "%Y%m%d")
mindex$von_datum <- as.Date(as.character(mindex$von_datum), "%Y%m%d")
mindex$bis_datum <- as.Date(as.character(mindex$bis_datum), "%Y%m%d")

m2 <- mindex[mindex$res=="annual" & mindex$var=="more_precip" & mindex$per=="historical" & mindex$hasfile,]
f2 <- findex[findex$res=="annual" & findex$var=="more_precip" & findex$per=="historical" & !is.na(findex$id),]
mf <- merge(m2[,c("Stations_id", "von_datum", "bis_datum")],
            f2[,c("id", "start", "end")], by.x="Stations_id", by.y="id")
rm(m2, f2)
mf$diff_von <- round(as.integer(mf$start - mf$von_datum)/365,2)
mf$diff_bis <- round(as.integer(mf$end   - mf$bis_datum)/365,2)
colnames(mf) <- gsub("_datum", "_meta", colnames(mf))
colnames(mf) <- gsub("start", "von_file", colnames(mf))
colnames(mf) <- gsub("end", "bis_file", colnames(mf))
mf[mf$diff_von >   5,]
mf[mf$diff_bis < -30,]
}



# gindex ----

if(!is.null(gindex)){
if(!quiet) message("Checking geoIndex...")
columns <- !colnames(gindex) %in% c("display","col")
# Duplicate coordinates checks:
# Exclude known false positives like "Dasburg" vs "Dasburg (WWV RLP)"
fpid <- c(# ID pairs:
          14306,921, 13967,13918, 14317,3024, 2158,7434, 785,787, 5248,5249, 
          2749,1495, 19579,604, 5840,19581, 1516,19626, 14351,19274, 1710,19659, 
          19980,2037, 396,397, 19645,2853, 2979,19658, 19706,3704, 19641,2457, 
          14320,3969, 4128,19662, 19664,4374, 4501,15232, 4892,19680, 5055,19694, 
          5162,19695, 5174,19580, 5435,19985, 5475,19698,
          # different names for 1 ID:
          19494, 141, 3234, 2667, 3297, 3238, 1052, 7372, 1888, 7077, 15526, 
          5985, 7492, 4080, 4352, 1803, 4412, 15003, 4618, 3881, 3509, 7078
          )
# View(gindex[gindex$id %in% fpid, columns]) # sort e.g. by lat
gindex_id <- gindex
if(excludefp) gindex_id <- gindex[!gindex$id %in% fpid,]
coord <- paste(gindex_id$lon, gindex_id$lat, sep="_")
# several stations at the same locations:
if(anyDuplicated(coord))
  {
  out <- c(out, "\nCoordinates used for more than one station:")
  new <- sapply(coord[duplicated(coord)], function(c){
    g <- gindex_id[coord==c, ]
    t <- toString(paste0(g$nfiles+g$nonpublic, "x ID=", g$id, " (", g$name, ")"))
    paste0("- ", c, ": ", t)
    })
  out <- c(out, new)
  }
}

# output stuff:
logfileprint <- if(!is.null(logfile)) paste0("  openFile('",
                  normalizePath(logfile,winslash="/", mustWork=FALSE),"')") else ""
if(length(out)>2 & warn) twarning("There are issues in the indexes.", logfileprint)
out <- c(out, "\n")
out <- paste(out, collapse="\n")
if(!is.null(logfile)) cat(out, file=logfile, append=TRUE)
return(invisible(out))
} # end checkIndex
