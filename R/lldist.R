#' distance between lat-long coordinates
#'
#' Great-circle distance between points at lat-long coordinates.
#' Mostly a copy of OSMscale::earthDist Version 0.4.5 (2017-03-02).
#' Copied manually to avoid dependency hell.
#' Does not check coordinates. Not exported.
#'
#' @return Vector with distance(s) in km (or units of \code{r}, if \code{r} is changed)
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2016 + Jan 2017.
#'         Angle formula from Diercke Weltatlas 1996, Page 245
#' @seealso \code{\link{degree}} for pre-formatting,
#'          \url{http://www.movable-type.co.uk/scripts/latlong.html}
#' @keywords spatial
#' @importFrom berryFunctions getColumn almost.equal
### @export
#' @examples
#' d <- read.table(header=TRUE, sep=",", text="
#' lat, long
#' 52.514687,  13.350012   # Berlin
#' 51.503162,  -0.131082   # London
#' 35.685024, 139.753365") # Tokio
#' lldist(lat, long, d)      # from Berlin to L and T: 928 and 8922 km
#' lldist(lat, long, d, i=2) # from London to B and T: 928 and 9562 km
#'
#'
#' @param lat,long Latitude (North/South) and longitude (East/West) coordinates in decimal degrees
#' @param data Optional: data.frame with the columns \code{lat} and \code{long}
#' @param r radius of the earth. Could be given in miles. DEFAULT: 6371 (km)
#' @param i Integer: Index element against which all coordinate pairs
#'          are computed. DEFAULT: 1
#' @param trace Logical: trace the coordinate check with \code{\link{checkLL}}?
#'        Should be set to FALSE in a \link{do.call} setting to avoid overhead
#'        computing time. DEFAULT: TRUE
#'
lldist <- function(
lat,
long,
data,
r=6371,
i=1L,
trace=TRUE
)
{
# Input coordinates:
if(!missing(data)) # get lat and long from data.frame
  {
  lat  <- getColumn(substitute(lat) , data, trace=trace)
  long <- getColumn(substitute(long), data, trace=trace)
  }
# coordinate control:
###checkLL(lat, long, trace=trace)
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
samepoint <- almost.equal(x2, x1) & almost.equal(y2, y1) # berryFunctions::almost.equal
angle[samepoint] <- 0 # again, to compensate numerical inaccuracies
# compute great-circle-distance:
r*angle
}
