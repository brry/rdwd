
stop("Don't source this document")



# readDWD.nc projection ----

url <- "daily/Project_TRY/pressure/PRED_199606_daymean.nc.gz"  #  5 MB
file <- dataDWD(url, base=gridbase, joinbf=TRUE, dir=localtestdir(), read=FALSE)
nc_ncdf <- readDWD(file, toraster=FALSE)

ncfile <- R.utils::gunzip(file, remove=FALSE, skip=TRUE)
nc_rast <- terra::rast(ncfile, band=1)

# extent
nc_range <- data.frame(range(nc_ncdf$lon), range(nc_ncdf$lat))
sf::sf_project("+proj=longlat +datum=WGS84", "+init=epsg:3034", nc_range) 
ec <- c(3602269, 4388061, 2243094, 3196182) # from nc lat/lon
eb <- c(3697532, 4311490, 2300230, 3139517) # from DEU ranges
ef <- c(3667000, 4389000, 2242000, 3181000) # final from visual adaptation
nc_proj <- projectRasterDWD(nc_rast, proj="+init=epsg:3034", extent=ef); 
terra::plot(nc_proj)  ; addBorders()



# markdownify source code ----

f <- "R/addBorders.R"
l <- readLines(f)
#gsub("\\code\{\\link\{+\}\}","[+()]", l[18])


install.packages("roxygen2md")
library(roxygen2md)
roxygen2md("none")    # after each step, check git changes
find_rd()
roxygen2md("simple")
find_rd()
roxygen2md("full")
find_rd()
?markdownify()


brack2tick <- function(x) gsub("()`]", "`()]", gsub("(\\[)(.*\\(\\))(\\])", "\\1`\\2`\\3", x), fixed=TRUE)
l <- "#' @description check() indexes (kkk). som text in [createIndex()]. and some [8]"
brack2tick(l)
rm(l)


f <- "R/checkIndex.R" ; writeLines(brack2tick(readLines(f)),f)


indextick <- function(f)
  {
  l <- readLines(f)
  for(i in c("index", "fileIndex", "metaIndex", "geoIndex", "gridIndex", 
             "formatIndex", "dwdbase", "gridbase", "EUR", "DEU", "dwdparams"))
     l <- gsub(paste0("[",i,"()]"), paste0("[`",i,"`]"), l, fixed=TRUE)
  writeLines(l, f)
  }
indextick("R/zzz.R")



# readDWD.stand fwf speed comp ------

data("formatIndex")
file <- dataDWD(selectDWD(id=10381, res="subdaily", var="standard_format", per="r"),
                dir=localtestdir(), read=FALSE)
width <- c(diff(as.numeric(formatIndex$Pos)),1)

fread_fwf <- function(file)
{
c_beg <- as.numeric(formatIndex$Pos)
c_end <- as.numeric(formatIndex$Pos)[-1L] - 1L
sf <-  data.table::fread(file, sep="\n", header=FALSE)
sf <- sf[ , lapply(seq_len(length(c_beg)),	function(i) substr(V1, c_beg[i], c_end[i]))]
sf
}
# times noted as average of ~5 manual runs, not counting first.
system.time( sf1 <- read.fwf(file, widths=width, stringsAsFactors=FALSE)   )# 18.51 secs # chr/int
system.time( sf2 <- fread_fwf(file)                                        )#  0.08 secs # all chr
system.time( sf3 <- readr::read_fwf(file, readr::fwf_widths(width), 
                                    readr::cols())                         )#  0.09 secs # chr/num
system.time( sf4 <- iotools::input.file(file, formatter=iotools::dstrfw, 
                   col_types=rep("character",length(width)), widths=width) )#  0.04 secs # all chr



# recent radar for false files on Server


# ~ recrad ----

#' @param recrad Logical (vector): does the \code{file} contain recent radolan binary files?
#'               See \code{\link{readDWD.recrad}}.
#'               DEFAULT: TRUE for each file ending in ".gz"

#recrad=grepl(         '.gz$', file),
#recrad      <- rep(recrad,      length.out=len)
#if(recrad[i]) return(readDWD.recrad(file[i], ...))


#' @title read recent dwd gridded radolan binary data
#' @description read recent gridded radolan binary data.
#' Intended to be called via \code{\link{readDWD}}.\cr
#' Used for data at \url{ftp://opendata.dwd.de/weather/radar/radolan/rw}
#' @return list with dat and meta
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2019. 
#' @seealso \code{\link{readDWD}}, \code{\link{readDWD.binary}}
#' @examples
#' \dontrun{ # Excluded from CRAN checks, but run in localtests
#' # File
#' RR_rad <- readDWD(RW_file)
#' RR_radp <- projectRasterDWD(RR_rad$data, extent="rw")
#' terra::plot(RR_radp, main=RR_rad$meta$date[1])
#' terra::plot(DEU, add=TRUE)
#' }
#' @param file      Name of file on harddrive, like e.g. 
#'                  DWDdata/ToDo
#' @param toraster  Logical: convert matrix to \code{\link[terra]{rast}}?
#'                  DEFAULT: TRUE
#' @param \dots     Further arguments passed to \code{\link{readRadarFile}}, 
#'                  i.e. \code{na} and \code{clutter}
readDWD.recrad <- function(file, toraster=TRUE, ...)
{
# Unpack file:
# TODO!
rfile <- file
# Read the actual binary file:
rb <- readRadarFile(rfile, ...)
if(!toraster) return(invisible(rb))
# else if toraster:
checkSuggestedPackage("terra", "rdwd:::readDWD.recrad with toraster=TRUE")
rb$dat <- terra::rast(rb$dat)
return(invisible(rb))
}




# readDWD.meta ----
# Development process and tests
# in october 2016, DWD slightly changed monthly/kl meta file column widths
# Here are alternative thoughs on how to automatize width detection
"
> spaces
[1]   6  15  24  25  26  27  28  29  30
[10]  31  32  33  34  38  39  40  41  42
[19]  50  51  52  53  60  65  66  67  68
[28]  69  70  71  72  73  74  75  76  77
[37]  78  79  80  81  82  83  84  85  86
[46]  87  88  89  90  91  92  93  94  95
[55]  96  97  98  99 100 101 119 120 121
[64] 122 123 124 125 126 127 128 129 130
[73] 131 132 133 134 135 136 137 138 139
[82] 140 141 142 143 144 145 146 147 148
[91] 149 150 151 152 153 154 155 156 157
[100] 158 159 160 161 162 163 164 165 166
[109] 167 168 169 170 171 172 173 174 175
[118] 176 177 178 179 180 181 182 183 184
[127] 185 186 187 188 189 190 191 192 193
[136] 194 195 196 197 198 199 200
> sb
[1]   1   2   3   4   5   6   7   8   9
[10]  10  12  21  30  31  32  33  34  35
[19]  36  37  38  39  40  41  45  46  47
[28]  48  49  57  58  59  60  67  72  73
[37]  74  75  76  77  78  79  80  81  82
[46]  83  84  85  86  87  88  89  90  91
[55]  92  93  94  95  96  97  98  99 100
[64] 101 102 103 104 105 106 107 108 126
[73] 127 128 129 130 131 132 133 134 135
[82] 136 137 138 139 140 141 142 143 144
[91] 145 146 147 148 149 150 151 152 153
[100] 154 155 156 157 158 159 160 161 162
[109] 163 164 165 166 167 168 169 170 171
[118] 172 173 174 175 176 177 178 179 180
[127] 181 182 183 184 185 186 187 188 189
[136] 190 191 192 193 194 195 196 197 198
[145] 199 200
"
#             .        .        ..                .          .      .                                        .                      .
#             6        15       24   -   34   38-42      50-53      60   65               -                101                 119-200
a="00001 18910101 19860630           478     47.8413    8.8493 Aach                                     Baden-Württemberg                                                                                  "
b="          1 19370101 19860630            478     47.8413    8.8493 Aach                                     Baden-Württemberg                                                                           "
#  1   -   10 12       21       30    -   41   45-49      57-60      67    72            -                  108                 126-200
#             *        *        **                *           *     *                                         *                       *
sa <- unlist(gregexpr(" ", a)) # monthly more_precip historical
sb <- unlist(gregexpr(" ", b)) # daily   kl          historical
sa[which(diff(sa)!=1)]
sa[which(diff(sa)!=1)+1]
sb[which(diff(sb)!=1)]
#
# Check a couple different styles with:
mf <- selectDWD(res=c(rep("hourly",3), "monthly", "daily"), var=c("cloudiness","solar","sun","kl","kl"),
                time=c(rep("r",4), "h"), meta=TRUE, current=TRUE)
m <- dataDWD(mf)
lapply(m, head)
#
# Also removed from readDWD (see note on selectDWD id argument):
#                ID           VON         BIS        HOEHE    LAT       LONG      NAME     BUNDESLAND
#colClasses <- c("character", "integer", "integer", "numeric","numeric","numeric","factor","factor")
# some meta files have no leading zeros, so this package uses integer all the time. # colClasses=colClasses



# check station coordinates: ----
data("metaIndex")
coord_ok <- pbsapply(unique(metaIndex$Stationsname), function(n)
{
 sel <- metaIndex$Stationsname==n
 lat <- metaIndex$geoBreite[sel]
 lon <- metaIndex$geoLaenge[sel]
 ele <- metaIndex$Stationshoehe[sel]
 d <- 6 # number of digits rounded to
 all(round(lat,d)==round(lat[1],d)  &  round(lon,d)==round(lon[1],d)  & ele==ele[1]  )
})
mean(coord_ok) # 79% is OK, 94.9 % with d=2, 98% with d=1
names(coord_ok[!coord_ok])


# some more checks:
mean(metaIndex$hasfile) # 72% has a file
length(unique(metaIndex$Stations_id)) # 5778 IDs (5660 in geoIndex)
hist(table(metaIndex$Stations_id), breaks=100, col="cadetblue", xlab="number of entries per ID")

checkdupli <- function(a,b, x=metaIndex)
{
 d <- tapply(x[,a], x[,b], unique)
 list( morethan1=d[sapply(d, length)!=1],   table=table(sapply(d, length)) )
}

checkdupli("Bundesland", "Stationsname") # $`Holzdorf (Flugplatz)` "Sachsen-Anhalt" "Brandenburg"
checkdupli("Stations_id", "Stationsname") # $Hoerstel 2254 15559
checkdupli("Stationsname", "Stations_id") # 53 with 2

data("geoIndex")
checkdupli("name", "id", geoIndex) # 44 with 2

sum(geoIndex$nfiles_coord) # 25482
hist(geoIndex$nfiles_coord, breaks=100, col="cadetblue", xlab="number of files per location")


# static maps -----

if(!requireNameSpace("OSMscale")) install.packages("OSMscale")
library("OSMscale")

# Map of all precipitation stations (metaindex):
if(!exists("map")) map <- pointsMap(geoBreite, geoLaenge, data=metaIndex, fx=0.28, fy=0.06)
pdf("DWDdata/RainfallStationsMap_2.pdf")
# pointsMap(geoBreite, geoLaenge, data=metaIndex, map=map, pch=NA, scale=FALSE)
plot(map)
scaleBar(map, x=0.05, y=0.03, abslen=200)
pp <- projectPoints(geoBreite, geoLaenge, data=metaIndex, to=posm())
points(pp[!metaIndex$hasfile,], col="red", pch=3)
points(pp[ metaIndex$hasfile,], col="blue", pch=3)
legend("bottomright", c("in matadata only", "file on FTP server"),
       col=c("red", "blue"), pch=3, bg="white")
title(main="DWD stations: data on ftp server", line=3)
dev.off()


# . map geoIndex ----

map <- pointsMap(lat, lon, data=geoIndex, fx=0.06, fy=0.06)
pdf("DWDdata/RainfallStationsMap_nfiles_2.pdf", width=5)
plot(map)
scaleBar(map, x=0.05, y=0.03, abslen=200)
geoIndex <- sortDF(geoIndex, "nfiles", decreasing=FALSE)
pp <- projectPoints(lat, lon, data=geoIndex, to=posm())
points(pp, cex=0.6)
colPoints(pp$x, pp$y, geoIndex$nfiles, cex=0.6, zlab="")
title(main="DWD stations: number of files on ftp server", line=3)
dev.off()


# . Time series duration:
# colPoints <- berryFunctions::colPoints
colPoints(geoLaenge, geoBreite, Stations_id, data=metaIndex, add=F, asp=1.5)
colPoints(geoLaenge, geoBreite, Stationshoehe, data=metaIndex, add=F, asp=1.5)
metaIndex$von_jahr <- metaIndex$von_datum/1e4
metaIndex$bis_jahr <- metaIndex$bis_datum/1e4
metaIndex$dauer <- metaIndex$bis_jahr - metaIndex$von_jahr
colPoints(geoLaenge, geoBreite, von_jahr, data=metaIndex, add=F, asp=1.5)
colPoints(geoLaenge, geoBreite, bis_jahr, data=metaIndex, add=F, asp=1.5)
colPoints(geoLaenge, geoBreite, dauer, data=metaIndex, add=F, asp=1.5)
hist(metaIndex$bis_jahr, breaks=50, col="purple")
hist(metaIndex$dauer, breaks=50, col="purple")
sum(metaIndex$dauer>50); mean(metaIndex$dauer>50)
# 45% of stations with more than 50 years of data (according to metadata)

