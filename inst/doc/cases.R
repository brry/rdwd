## ----hourlyrain_data_selection, warning=FALSE----------------------------
library(rdwd)
links <- selectDWD(res="daily", var="more_precip", per="hist")
length(links) # 5583 stations - would take very long to download

# select only the relevant files:
data("metaIndex")
myIndex <- metaIndex[
  metaIndex$von_datum < 20140101 &
  metaIndex$bis_datum > 20161231 & metaIndex$hasfile   ,  ]
data("fileIndex")    
links <- fileIndex[
  as.numeric(fileIndex$id) %in% myIndex$Stations_id &
  fileIndex$res=="daily" &
  fileIndex$var=="more_precip" &
  fileIndex$per=="historical"         , "path" ]  
links <- paste0("ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/", links)

length(links) # 2001 elements - much better

## ----hourlyrain_data_download, message=FALSE-----------------------------
localfiles <- dataDWD(links[1:3], dir=tempdir(), sleep=0.2, read=FALSE)

## ----hourlyrain_reading_function-----------------------------------------
readVars(localfiles[1])[,-3] # we want the RS column

read2014_2016 <- function(file, fread=TRUE, ...)
{
 out <- readDWD(file, fread=fread, ...)
 out <- out[out$MESS_DATUM > as.POSIXct(as.Date("2014-01-01")) & 
            out$MESS_DATUM < as.POSIXct(as.Date("2016-12-31"))    , ]
 out <- out[ , c("MESS_DATUM", "RS")]
 out$MESS_DATUM <- as.Date(out$MESS_DATUM) # might save some memory space...
 # Station id as column name:
 idstringloc <- unlist(gregexpr(pattern="tageswerte_RR_", file))
 idstring <- substring(file, idstringloc+14, idstringloc+18)
 colnames(out) <- c("date",  idstring)
 return(out)
}
str(read2014_2016(localfiles[1])) # test looks good

## ----hourlyrain_data_reading---------------------------------------------
if(!requireNamespace("pbapply")) install.packages("pbapply")
library(pbapply) # progress bar for lapply loop

rain_list <- pblapply(localfiles, read2014_2016)
rain_df <- Reduce(function(...) merge(..., all=T), rain_list)
str(rain_df) # looks nice!
summary(rain_df) # 6 NAs in station 00006

## ----hourlyrain_vis------------------------------------------------------
plot(rain_df$date, rain_df[,2], type="n", ylim=range(rain_df[,-1], na.rm=T), 
     las=1, xaxt="n", xlab="Date", ylab="Daily rainfall sum  [mm]")
berryFunctions::monthAxis()
for(i in 2:ncol(rain_df)) lines(rain_df$date, rain_df[,i], col=sample(colours(), size=1))

plot(rain_df[,2:4]) # correlation plot only works for a few columns!

## ----hourlyrain_map_interactive, warning=FALSE---------------------------
if(requireNamespace("leaflet")){
data(geoIndex)  ;  library(leaflet) 
mygeoIndex <- geoIndex[geoIndex$id %in% as.numeric(colnames(rain_df)),]

leaflet(data=mygeoIndex) %>% addTiles() %>%
        addCircleMarkers(~lon, ~lat, popup=~display, stroke=T)
}

## ----hourlyrain_map_static, message=FALSE--------------------------------
if(requireNamespace("OSMscale")){
library(OSMscale)
pointsMap("lat", "lon", mygeoIndex, fx=2, fy=1, pargs=list(lwd=3), 
                    col="blue", zoom=5)
}

