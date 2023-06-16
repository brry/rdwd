library(rdwd)
library(berryFunctions)
data("geoIndex")
if(!exists("ws")) ws <- readxl::read_excel("misc/hex/weather_symbol.xlsx", col_types="numeric")
if(!exists("map")){
map <- geodata::world(path=locdir())
map <- terra::crop(map, c(0,20, 40,60)) # for faster plotting
}


{
pdf("hexraw.pdf", width=7, height=7*600/518*1.05)
par(bg="cadetblue1", mar=rep(0,4))
plot(lat~lon, data=geoIndex, type="n", axes=FALSE, xlab="", ylab="", asp=1.5,
     xlim=c(3,18), ylim=c(45,57))
terra::plot(map, add=TRUE, border="gray70", col="gray80", lwd=6)
points(lat~lon, data=geoIndex[geoIndex$recentfile,], col="red", cex=1.2, pch=3)
lines(berryFunctions::rescale(ws$x, 7.4, 11.4)+0.2, 
      berryFunctions::rescale(ws$y, 49.2, 51.7)-0.6, lwd=11)
text(10.7, 53-0.5, "rdwd", cex=7, font=2)
dev.off()
}

pdf <- F # if FALSE: png

hexSticker::sticker("hexraw.pdf", s_x=1, s_y=1, s_width=1, s_height=1, asp=0.85,
                    package="rdwd", p_color="transparent",
                    h_fill="transparent", h_size=0, h_color="transparent",
                    url="github.com/brry/rdwd", u_size=ifelse(pdf,2.3,7.2),
                    white_around_sticker=TRUE,
                    filename=ifelse(pdf,"hex.pdf","hex.png"))
