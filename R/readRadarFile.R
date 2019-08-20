#' @title read binary radolan radar file
#' @description Read a single binary DWD Radolan file.
#'   To be used in \code{\link{readDWD.binary}}.\cr
#'   If any files ar not read correctly, please let me know. So far, tests have 
#'   only been conducted for some files. Optimally, check the 
#'   Kompositformatbeschreibung at \url{https://www.dwd.de/DE/leistungen/radolan/radolan.html}
#'   and let me know what needs to be changed.\cr
#'   Binary bits are converted to decimal numbers with Fortran routines, see
#'   \url{https://github.com/brry/rdwd/tree/master/src}. 
#'   They are called in an unexported function called \code{bin2num}.
#' @return Invisible list with \code{dat} (matrix) and \code{meta} 
#' (list with elements from header, see Kompositformatbeschreibung)
#' @author Maintained by Berry Boessenkool, \email{berry-b@@gmx.de}, May 2019.\cr
#'   Original codebase by Henning Rust & Christoph Ritschel at FU Berlin
#' @seealso \code{\link{readDWD.binary}}, \code{\link{readDWD.radar}}
#' @keywords file binary
#' @useDynLib rdwd, .registration=TRUE
# @importFrom package fun1 fun2
#' @export
#' @examples
#' # See readDWD.radar and readDWD.binary
#' 
#' @param binfile Name of a single binary file
#' @param na      Value to be set for missing data (bit 14). DEFAULT: NA
#' @param clutter Value to be set for clutter data (bit 16). DEFAULT: NA
#'
readRadarFile <- function(binfile, na=NA, clutter=NA)
{
finalOut <- try({
openfile <- file(binfile,"rb") # will be read successively
on.exit(close(openfile), add=TRUE)
# helper function to read elements of the header:
readheader <- function(n, confile=openfile, asnum=FALSE)
 {
 out <- rawToChar(readBin(confile,what=raw(),n=n,endian="little"))
 if(asnum) out <- as.numeric(out)
 out
 }
# header of first file in /daily/radolan/historical/bin/2017/SF201712.tar.gz
# SF010050100001217BY1620267VS 3SW   2.16.0PR E-01INT1440GP 900x 900MS 
# 70<boo,ros,emd,hnr,umd,pro,ess,fld,drs,neu,nhb,oft,eis,tur,isn,fbg,mem> ST
# 120<boo 24,drs 24,eis 24,emd 24,ess 24,fbg 24,fld 24,hnr 24,isn 24,mem 24,neu 24,
#   nhb 24,oft 24,pro 24,ros 24,tur 24,umd 24>ETX
PRODUCT  <- readheader(2) # SF
rw <- PRODUCT=="RW"
DDHHMM   <- readheader(6) # 010050
LOCATION <- readheader(5) # 10000
MMYY     <- readheader(4) # 1217
BY       <- readheader(2) # BY
LENGTH   <- readheader(7, asnum=TRUE) # 1620267 Bytes
ID       <- readheader(2) # VS
FORMATV  <- readheader(2) # " 3"
SW       <- readheader(2) # SW
VER      <- readheader(9) # "   2.16.0"
PR       <- readheader(2) # PR
PREC     <- as.numeric(sub("^ ", "1", readheader(5))) # " E-01" to 0.1
INT      <- readheader(3) # INT
DT       <- readheader(4, asnum=TRUE) # 1440 minutes
U0 <- if(rw)readheader(2) else ""
if(U0!="GP")
GP       <- readheader(2) # GP
DIM      <- as.numeric(unlist(strsplit(readheader(9),"x"))) # " 900x 900" to c(900,900)
MS       <- readheader(2) # MS
if(rw) VR<- readheader(11) # " 00000001MS" / " 00000001VR2017.002MS"
if(rw) if(substr(VR,10,11)=="VR") VR2 <- readheader(10)
TLEN     <- readheader(3, asnum=TRUE) # 70 characters
RADS     <- unlist(strsplit(gsub("<|>| ","",readheader(TLEN)),",")) # Radarstandortkuerzel (boo, ros, emd, ...)
if(rw | PRODUCT=="RX")
  {  ETX <- readheader(1) ; RADB <- NA } else {
ST       <- readheader(2) # ST
TLEN2    <- readheader(3, asnum=TRUE) # 120 characters
RADB     <- unlist(strsplit(gsub("<|>|","",readheader(TLEN2)),",")) # similar to rads
ETX      <- readheader(1) # "\003" End of Text 
  } # end not rw

if(ETX!="\003") stop("rdwd::readRadarFile: header could not be read correctly.",
           " Please send the DWD file name to berry-b@gmx.de", call.=FALSE)

LEN <- DIM[1]*DIM[2]
# read the remaining binary data set:
dat <- readBin(openfile, what=raw(), n=LEN*2, endian="little")

# convert into a two byte set and then into values with fortran routines:
if(PRODUCT=="RX") # WX,RX,EX?
  {
  dim(dat) <- c(1,LEN)
  dat.val <- bin2num(dat,LEN,na,clutter, RX=TRUE) # see function definition below
  dat.val <- dat.val/2 - 32.5
  }else 
  # for SF (and RW?)
  {
  dim(dat) <- c(2,LEN)
  dat.val <- bin2num(dat,LEN,na,clutter)
  }
  
# apply precision given in the header:
dat.val <- dat.val*PREC

# convert into a matrix + give row and column names according to RADOLAN convention:
if(rw | PRODUCT=="SF")
  {
  dat.mat <- matrix(dat.val, ncol=DIM[2], byrow=TRUE) # ToDo: not sure about this
  dat.mat <- apply(dat.mat, 2, rev)
  dimnames(dat.mat) <- list(x.nrs=1:DIM[1]-1, y.nrs=1:DIM[2]-1)
  }
else
  {
  dat.mat <- t(matrix(dat.val,DIM[1],DIM[2])) # i=lon, j=lat
  dimnames(dat.mat) <- list(x.nrs=1:DIM[2]-1, y.nrs=1:DIM[1]-1) # reversed because of t()
  }

# meta data:
daytime <- strptime(paste0(DDHHMM,"00-",MMYY), format="%d%H%M%S-%m%y")
meta <- list(filename=binfile, date=daytime, product=PRODUCT, 
             location=LOCATION, id_vs=ID, radolan_version=VER, 
             precision=PREC, interval_minutes=DT, dim=DIM, 
             radius_format=FORMATV, radars=RADS, radarn=RADB)
return(list(dat=dat.mat, meta=meta))
}, silent=TRUE) # end of try
if(inherits(finalOut,"try-error")) warning(finalOut, "in file: ", binfile, call.=FALSE)
return(invisible(finalOut))
}





# non-exported + non-documented helper function
bin2num <- function(dat, len, na=NA, clutter=NA, RX=FALSE) 
{
Fna <- -32767L
Fclutter <- -32766L
if(RX) {
  na <- (na+32.5)*2
  clutter <- (clutter+32.5)*2
  out <- .Fortran("binary_to_num_rx", raw=dat, Flength=as.integer(len),
                numeric=as.integer(array(0,dim=len)), Fna=Fna, Fclutter=Fclutter)
  }
else
out <- .Fortran("binary_to_num", raw=dat, Flength=as.integer(len),
                numeric=as.integer(array(0,dim=len)), Fna=Fna, Fclutter=Fclutter)
out <- out$numeric
out[out==Fna] <- na
out[out==Fclutter] <- clutter
return(out)
}
