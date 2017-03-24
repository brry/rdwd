#' Find DWD stations close to given coordinates
#'
#' Select DWD stations within a given radius around a set of coordinates
#'
#' @return \code{\link{metaIndex}} subset with additional columns "dist" and "url"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2017
#' @seealso \code{\link{selectDWD}}, \code{\link{metaIndex}}
#' @export
#' @examples
#'
#' # 1. Basic usage ----
#'
#' m <- nearbyStations(49.211784, 9.812475, radius=30,
#'     res=c("daily","hourly"), var= c("precipitation","more_precip","kl") ,
#'     mindate=20160530, statname="Braunsbach catchment center")
#'
#'
#' # 2. Remove duplicates ----
#'
#'# if kl and more_precip are both available, keep only more_precip:
#' library("berryFunctions")
#' m <- sortDF(m, "var")
#' m <- m[!duplicated(paste0(m$Stations_id, m$res)),]
#' m <- sortDF(m, "res")
#' m <- sortDF(m, "dist", decreasing=FALSE)
#' rownames(m) <- NULL
#'
#'
#' ## 3. Interactive map ----
#'
#' \dontrun{## Excluded from CRAN checks because of leaflet dependency
#' library(leaflet)
#' m$col <- "red" ; m$col[1] <- "blue"
#' leaflet(m) %>% addTiles() %>%
#'   addCircles(lng=9.812475, lat=49.211784, radius=30e3) %>%
#'   addCircleMarkers(~geoLaenge, ~geoBreite, col=~col, popup=~Stationsname)
#' }
#'
#'
#' ## 4. Download and process data ----
#'
#' \dontrun{## Excluded from CRAN checks because of data download
#' # Download and process data for the stations, create a list of data.frames:
#' prec <- dataDWD(m$url) # once downloaded, will only read
#' names(prec) <- m$Stations_id[-1]
#' prec29 <- sapply(prec[m$res[-1]=="daily"], function(x)
#'          x[x$MESS_DATUM==as.POSIXct(as.Date("2016-05-29")), c("STATIONS_ID","NIEDERSCHLAGSHOEHE")])
#' prec29 <- data.frame(Stations_id=unlist(prec29[1,]), precsum=unlist(prec29[2,]))
#' prec29 <- merge(prec29, m[m$res=="daily",c(1,4:7,14)], sort=FALSE)
#' View(prec29)
#' }
#'
#'
#' ## 5. Plot rainfall sum on map
#'
#' \dontrun{## Excluded from CRAN checks because of map download
#' plot(geoBreite~geoLaenge, data=m, asp=1)
#' textField(prec29$geoLaenge, prec29$geoBreite, prec29$precsum, col=2)
#'
#' # If OSMscale installation fails, go to:
#' browseURL("https://github.com/brry/OSMscale#installation")
#' # install.packages("OSMscale")
#' library(OSMscale)
#' map <- pointsMap(geoBreite,geoLaenge, data=m, type="maptoolkit-topo")
#' pp <- projectPoints("geoBreite", "geoLaenge", data=prec29, to=map$tiles[[1]]$projection)
#' prec29 <- cbind(prec29,pp) ; rm(pp)
#' plot(map, removeMargin=FALSE)
#' scaleBar(map, cex=1.5, type="line", y=0.82)
#' title(main="Rainfall sum  2016-05-29  7AM-7AM  [mm]", line=-1)
#' textField(prec29$x, prec29$y, round(prec29$precsum), font=2, cex=1.5)
#' }
#'
#' @param lat,lon     Coordinates [degrees N/S, E/W]
#' @param radius      Maximum distance [km] within which stations will be selected
#' @param res,var,per Restrictions for dataset type as documented in
#'                    \code{\link{selectDWD}}. Each can be a vector of entries.
#'                    DEFAULTS: NA (ignored)
#' @param mindate     Minimum dataset ending date (as per metadata).
#'                    Integer in the form of YYYYMMDD, e.g. 20170301. DEFAULT: NA
#' @param hasfileonly Logical: only return entries for which there is an
#'                    open-access file available? DEFAULT: TRUE
#' @param statname    Character: name for target locatio. DEFAULT:
#'                    "nearbyStations target location"
#' @param quiet       Logical: suppress progress messages? DEFAULT: FALSE
#' @param \dots       Further arguments passed to \code{\link{selectDWD}}
#'
nearbyStations <- function(
lat,
lon,
radius,
res=NA,
var=NA,
per=NA,
mindate=NA,
hasfileonly=TRUE,
statname="nearbyStations target location",
quiet=FALSE,
...
)
{
# input checks:
if(length(lat)>1) stop("lat mus be a single value, not ", length(lat))
if(length(lon)>1) stop("lon mus be a single value, not ", length(lon))
if(length(mindate)>1) stop("mindate mus be a single value, not ", length(mindate))
if(radius>1000) warning("radius is supposed to be given in km. ",
                        "Your value seems irreasonably high: ", radius)

if(!quiet) message("Selecting stations...")
m <- metaIndex
# Select restrictively for res/var/per, file availability, minimum end date:
if(hasfileonly) m <- m[m$hasfile,]
if(any(!is.na(res))) m <- m[m$res %in% res,]
if(any(!is.na(var))) m <- m[m$var %in% var,]
if(any(!is.na(per))) m <- m[m$per %in% per,]
if(!is.na(mindate))  m <- m[m$bis_datum>=mindate,]

# prepended row in data.frame:
m <- rbind(NA, m)
m$geoBreite[1] <- lat
m$geoLaenge[1] <- lon
m$Stationsname[1] <- statname
m$res[1] <- "z" # for later decreasing sorting
rownames(m)[1] <- "Target"

# compute distances:
m$dist <- lldist("geoBreite","geoLaenge", data=m)
# actually select closeby stations:
m <- m[m$dist<radius,]
m <- sortDF(m, "dist", decreasing=FALSE)

# add downloading URLS that can be passed to dataDWD:
if(!quiet) message("Preparing URLs...")
m$url <- ""
m$url[-1] <- with(m[-1,],
               selectDWD(id=Stations_id, res=res, var=var,per=per, outvec=TRUE, ...)  )
# final output:
return(m)
}
