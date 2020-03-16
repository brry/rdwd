#' distance between lat-long coordinates
#' 
#' Great-circle distance between points at lat-long coordinates.
#' Mostly a copy of OSMscale::earthDist Version 0.5.3 (2017-04-19).
#' \url{https://github.com/brry/OSMscale/blob/master/R/earthDist.R#L57-L102}.
#' Copied manually to avoid dependency hell.
#' Does not check coordinates. Not exported.
#' 
#' @return Vector with distance(s) in km (or units of \code{r}, if \code{r} is changed)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2016 + Jan 2017.
#'         Angle formula from Diercke Weltatlas 1996, Page 245
#' @keywords spatial
#' @importFrom berryFunctions getColumn
#' 
#' @param lat,long Latitude (North/South) and longitude (East/West) coordinates in decimal degrees
#' @param data Optional: data.frame with the columns \code{lat} and \code{long}
#' @param r radius of the earth. Could be given in miles. DEFAULT: 6371 (km)
#' @param i Integer: Index element against which all coordinate pairs
#'          are computed. DEFAULT: 1
#' 
lldist <- function(
lat,
long,
data,
r=6371,
i=1L
)
{
# Input coordinates:
if(!missing(data)) # get lat and long from data.frame
  {
  lat  <- getColumn(substitute(lat) , data)
  long <- getColumn(substitute(long), data)
  }
# index control:
i <- as.integer(i[1])
# convert degree angles to radians
y1 <-  lat[i]*pi/180
x1 <- long[i]*pi/180
y2 <-  lat*pi/180
x2 <- long*pi/180
# angle preparation (numerical inaccuracies may lead to 1.0000000000000002):
cosinusangle <- sin(y1)*sin(y2) + cos(y1)*cos(y2)*cos(x1-x2)
cosinusangle <- replace(cosinusangle, cosinusangle>1, 1)
# angle between lines from earth center to coordinates:
angle <- acos( cosinusangle )
# set distance between the same coordinates to exactly zero:
tol <- sqrt(.Machine$double.eps) # equality tolerance
samepoint <-    abs(x2-x1) < tol  &   abs(y2-y1) < tol
angle[samepoint] <- 0 # again, to compensate numerical inaccuracies
# compute great-circle-distance:
r*angle
}

#' @rdname lldist
#' @param fun   Function to be applied. DEFAULT: \code{\link{max}}
#' @param each  Logical: give max dist to all other points for each point separately?
#'              If FALSE, will return the maximum of the complete distance matrix,
#'              as if \code{max(maxlldist(y,x))}. For examples, see
#'              \href{https://github.com/brry/OSMscale/blob/master/R/maxEarthDist.R#L14-L33}{OSMscale::maxEarthDist}
#'              DEFAULT: TRUE
#' @param \dots Further arguments passed to fun, like na.rm=TRUE
#' 
maxlldist <- function(
lat,
long,
data,
r=6371,
fun=max,
each=TRUE,
...
)
{
if(!missing(data)) # get lat and long from data.frame
  {
  lat  <- getColumn(substitute(lat) , data)
  long <- getColumn(substitute(long), data)
  }
if(length(lat)==1) d <- 0 else # NA instead of 0 considered but rejected for now.
d <- sapply(seq_along(lat), function(i) lldist(lat,long,r=r,i=i)[-i] )
if(!each) return(  fun(d, ...)  )   # d[upper.tri(d)]
if(NCOL(d)==1) return(unlist(d)) # if only one or two points are compared
apply(d, 2, fun, ...)
}
