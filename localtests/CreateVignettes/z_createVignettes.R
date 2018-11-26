
# CRAN regularly rebuilds vignettes. To reduce the load on the DWD Server,
# I have decided Nov 2018 to precompile the vignettes and include only the output html on CRAN.
# Check out the Vignette rmd source codes at
# https://github.com/brry/rdwd/tree/master/localtests/CreateVignettes
 

# Note to Future Berry: just source this entire file and you're done.

if(getwd() != "S:/Dropbox/Rpack/rdwd") 
  stop("getwd should be S:/Dropbox/Rpack/rdwd, but is: ", getwd()  )
 
# Function to automate vignette compilation:
createBerrysVignettes <- function(file, title)
  {
  # knit to html
  vig_file <- rmarkdown::render(file, output_dir="vignettes", envir=new.env())
  #
  # html.asis file, see:
  # https://cran.r-project.org/web/packages/R.rsp/vignettes/R_packages-Static_PDF_and_HTML_vignettes.pdf
  #
  asis_file <- paste0("vignettes/", tools::file_path_sans_ext(basename(file)), ".html.asis")
  cat(paste0("
%\\VignetteIndexEntry{", title, "}
%\\VignetteEngine{R.rsp::asis}
%\\VignetteKeyword{HTML}
%\\VignetteKeyword{vignette}
%\\VignetteKeyword{package}\n"), file=asis_file)
  # 
  # inform:
  message("Created 2 files: '", vig_file, "', '", asis_file, "'.")
  }


# actual usage:

createBerrysVignettes(file="localtests/CreateVignettes/rdwd.Rmd",
                      title="rdwd: climate data from the German Weather Service")

createBerrysVignettes(file="localtests/CreateVignettes/mapDWD.Rmd",
                      title="DWD weather stations map")

createBerrysVignettes(file="localtests/CreateVignettes/cases.Rmd",
                      title="rdwd use cases")

devtools::build_vignettes()
