context("Station coordinates")

## TODO: make real tests

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


if(FALSE){

# check coordinates:
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

checkdupli("name", "id", geoIndex) # 44 with 2

sum(geoIndex$nfiles_coord) # 25482
hist(geoIndex$nfiles_coord, breaks=100, col="cadetblue", xlab="number of files per location")


# static maps -----

if(!requireNameSpace("OSMscale")) install.packages("OSMscale")
library("OSMscale")

# Map of all precipitation stations (metaindex):
map <- pointsMap(geoBreite, geoLaenge, data=metaIndex, fx=0.28, fy=0.06)
pdf("DWDdata/RainfallStationsMap.pdf")
plot(map)
scaleBar(map, x=0.05, y=0.03, abslen=200)
pp <- projectPoints(geoBreite, geoLaenge, data=metaIndex, to=posm())
points(pp[!metaIndex$hasfile,], col="red", pch=3)
points(pp[ metaIndex$hasfile,], col="blue", pch=3)
legend("bottomright", c("in matadata only", "file on FTP server"),
       col=c("red", "blue"), pch=3, bg="white")
title(main="DWD stations: data on ftp server", line=3)
dev.off()


# map geoIndex

map <- pointsMap(lat, long, data=geoIndex, fx=0.06, fy=0.06)
pdf("DWDdata/RainfallStationsMap_nfiles.pdf", width=5)
plot(map)
scaleBar(map, x=0.05, y=0.03, abslen=200)
geoIndex <- sortDF(geoIndex, "nfiles_coord", decreasing=FALSE)
pp <- projectPoints(lat, long, data=geoIndex, to=posm())
points(pp, cex=0.6)
colPoints(pp$x, pp$y, geoIndex$nfiles_coord, cex=0.6, zlab="")
title(main="DWD stations: number of files on ftp server", line=3)
dev.off()


# Time series duration:
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

}


