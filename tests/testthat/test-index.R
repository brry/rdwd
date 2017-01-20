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

}


