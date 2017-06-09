## ----map, fig.height=7, fig.width=7, warning=FALSE-----------------------
library(rdwd)  ;  data(geoIndex)  ;  library(leaflet) 
leaflet(geoIndex) %>% addTiles() %>%
        addCircles(~lon, ~lat, radius=900, stroke=F, color=~col)%>%
        addCircleMarkers(~lon, ~lat, popup=~display, stroke=F, color=~col)

## ----vignette_local, eval=FALSE------------------------------------------
#  vignette("mapDWD")

## ----onlyrecent, eval=FALSE----------------------------------------------
#  library(rdwd)  ;  data(geoIndex)  ;  library(leaflet)
#  leaflet(data=geoIndex[geoIndex$recentfile,]) %>% addTiles() %>%
#          addCircleMarkers(~lon, ~lat, popup=~display, stroke=F)

