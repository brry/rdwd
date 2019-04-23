
# CRAN regularly rebuilds vignettes. To reduce the load on the DWD Server,
# I have decided Nov 2018 to precompile the vignettes and include only the output html on CRAN.
# Check out the Vignette rmd source codes at
# https://github.com/brry/rdwd/tree/master/localtests/CreateVignettes
 

# Note to Future Berry: just source this entire file and you're done.

install_with_buildvignettes <- TRUE
begintime <- Sys.time()

if(substr(getwd(),2,100) != ":/Dropbox/Rpack/rdwd") 
  stop("getwd should be S:/Dropbox/Rpack/rdwd, but is: ", getwd()  )
 

# vignette files to render:
vigfiles <- read.table(stringsAsFactors=FALSE, sep="/", text="
rdwd.Rmd   ; rdwd: climate data from the German Weather Service
cases.Rmd  ; rdwd use cases
mapDWD.Rmd ; DWD weather stations map")


# Function to automate vignette compilation:
# equivalent in function to devtools::build_vignettes()
createBerrysVignettes <- function(rownumber)
  {
  ft <- strsplit(vigfiles[rownumber, 1], ";")[[1]]
  file  <- berryFunctions::removeSpace(ft[1])
  title <- berryFunctions::removeSpace(ft[2])
  # filenames:
  fname <- tools::file_path_sans_ext(basename(file))
  f2render <- paste0("localtests/CreateVignettes/",  file)
  f_vig    <- paste0("vignettes/", fname, ".html")
  f_asis   <- paste0("vignettes/", fname, ".html.asis")
  # knit to html
  frendered <- rmarkdown::render(f2render, output_dir=tempdir(), envir=new.env())
  # rendered in tempdir to avoid cases_files folder
  file.copy(frendered, f_vig, overwrite=TRUE)
  # html.asis file, see:
  # https://cran.r-project.org/web/packages/R.rsp/vignettes/R_packages-Static_PDF_and_HTML_vignettes.pdf
  cat(paste0("
%\\VignetteIndexEntry{", title, "}
%\\VignetteEngine{R.rsp::asis}
%\\VignetteKeyword{HTML}
%\\VignetteKeyword{vignette}
%\\VignetteKeyword{package}\n"), file=f_asis)
  }


nv <- nrow(vigfiles)
library(pbapply); library(parallel) # for parallel lapply execution
nc <- detectCores()
if(nc>nv) nc <- nv
cl <- makeCluster(nc)
clusterExport(cl, "vigfiles")
dummy <- pblapply(X=1:nv, cl=cl, FUN=createBerrysVignettes)
stopCluster(cl); gc()


if(install_with_buildvignettes) devtools::install(build_vignettes=TRUE)

message("Vignette creation finished!  Total run time: ", 
        round(difftime(Sys.time(), begintime, units="min"),1), " minutes")

.rs.restartR()
