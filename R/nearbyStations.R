#' Find DWD stations close to given coordinates
#' 
#' Select DWD stations within a given radius around a set of coordinates
#' 
#' @return \code{\link{metaIndex}} subset with additional columns "dist" and "url"
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Mar 2017
#' @seealso \code{\link{selectDWD}}, \code{\link{metaIndex}}
#' @export
#' @importFrom berryFunctions sortDF
#' @examples
#' 
#' m <- nearbyStations(49.211784, 9.812475, radius=30,
#'     res=c("daily","hourly"), var= c("precipitation","more_precip","kl") ,
#'     mindate=as.Date("2016-05-30"), statname="Braunsbach catchment center")
#' # View(m)
#'     
#' # for a continued example of this, see the vignette in chapter
#' # use case: plot all rainfall values around a given point
#' # browseURL("https://bookdown.org/brry/rdwd")
#' 
#' @param lat         Coordinates y component [degrees N/S, range 47:55]
#' @param lon         Coordinates x component [degrees E/W, range 6:15]
#' @param radius      Maximum distance [km] within which stations will be selected
#' @param res,var,per Restrictions for dataset type as documented in
#'                    \code{\link{selectDWD}}. Each can be a vector of entries.
#'                    DEFAULTS: NA (ignored)
#' @param mindate     Minimum dataset ending date (as per metadata). DEFAULT: NA
#' @param hasfileonly Logical: only return entries for which there is an
#'                    open-access file available? DEFAULT: TRUE
#' @param statname    Character: name for target location. DEFAULT:
#'                    "nearbyStations target location"
#' @param quiet       Logical: suppress progress messages? DEFAULT: FALSE through \code{\link{rdwdquiet}()}
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
quiet=rdwdquiet(),
...
)
{
# input checks:
if(length(lat)>1) stop("lat mus be a single value, not ", length(lat))
if(length(lon)>1) stop("lon mus be a single value, not ", length(lon))
if(any(lon>lat)) warning("lon>lat, but lon should be smaller than lat in Germany.",
                         immediate.=TRUE)
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

if(nrow(m)<=1) stop("No data was found for this query. Try increasing the radius.")

# add downloading URLS that can be passed to dataDWD:
if(!quiet) message("Preparing URLs...")
m$url <- ""
m$url[-1] <- with(m[-1,],
               selectDWD(id=Stations_id, res=res, var=var,per=per, outvec=TRUE, ...)  )
# final output:
return(m)
}
