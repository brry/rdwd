## ----map, fig.height=7, fig.width=7, warning=FALSE-----------------------
library(rdwd)  ;  data(geoIndex)  ;  library(leaflet) 
leaflet(geoIndex) %>% addTiles() %>%
        addCircles(~long, ~lat, radius=900, stroke=F, color=~col)%>%
        addCircleMarkers(~long, ~lat, popup=~display, stroke=F, color=~col)

## ----vignette_local, eval=FALSE------------------------------------------
#  vignette("mapDWD")

## ----onlyrecent, eval=FALSE----------------------------------------------
#  library(rdwd)  ;  library(leaflet)
#  data("geoIndex")
#  leaflet(data=geoIndex[geoIndex$recentfile,]) %>% addTiles() %>%
#          addCircleMarkers(~long, ~lat, popup=~display, stroke=F)

## ----all, eval=FALSE-----------------------------------------------------
#  data("geoIndexAll")
#  leaflet(data=geoIndexAll) %>% addTiles() %>%
#          addCircleMarkers(~long, ~lat, popup=~display, stroke=F)

