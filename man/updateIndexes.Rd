% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateIndexes.R
\name{updateIndexes}
\alias{updateIndexes}
\title{update rdwd indexes}
\usage{
updateIndexes(dwdlocal = FALSE, grdlocal = FALSE, metaforce = NA)
}
\arguments{
\item{dwdlocal}{Read "DWDdata/INDEX_of_DWD_.txt" instead of calling
\code{\link[=indexFTP]{indexFTP()}}? DEFAULT: FALSE}

\item{grdlocal}{Read "DWDdata/INDEX_of_DWD_grids.txt" instead of calling
\code{\link[=indexFTP]{indexFTP()}}? DEFAULT: FALSE}

\item{metaforce}{\code{\link[=dataDWD]{dataDWD()}} \code{force} argument for BESCHREIBUNG files.
DEFAULT: NA (re-download if older than 24 hours)}
}
\value{
\code{\link[=checkIndex]{checkIndex()}} results
}
\description{
This is meant to be called with getwd at the
rdwd package source code directory to update the indexes with one single call.\cr
To use custom or current indexes, see \url{https://bookdown.org/brry/rdwd/fileindex.html}
}
\examples{
# number of files at dwdbase
#  25'757 (2017-03-14)
# 218'593 (2018-03-25)
# 228'830 (2018-11-26)
# 240'737 (2019-02-19)
# 242'584 (2019-03-11)
# 266'860 (2019-05-15)
# 254'446 (2019-05-30)
# 255'252 (2019-07-31)
# 254'925 (2019-09-17)
# 254'943 (2019-10-26)
# 266'841 (2020-03-16)
# 265'712 (2020-04-10)
# 265'712 (2020-04-24)
# 266'106 (2020-06-01)
# 266'216 (2020-07-06)
# 266'216 (2020-07-28)
# 267'175 (2020-09-21)
# 269'561 (2020-12-03)
# 286'306 (2021-04-02)
# 286'189 (2021-04-08)
# 285'246 (2021-04-23)
# 285'972 (2021-06-02)
# 321'477 (2022-04-07)
# 477'236 (2022-04-28) # 5_minutes files added
# 482'907 (2022-04-29)
# 497'190 (2022-05-13)
# 576'768 (2023-04-06) # lots of duplicate historical files in transition period
# 577'160 (2023-04-12)
# 571'667 (2023-04-14)
# 572'583 (2023-05-10)
# 574'397 (2023-06-16)
# 573'963 (2023-09-16)
# 588'527 (2024-04-09)
# 586'626 (2024-05-14)
# 609'082 (2024-08-12)
# 596'417 (2025-01-23)
# 630'152 (2025-03-10)
# 633'334 (2025-05-20)
# 632'723 (2025-06-03)
# 634'195 (2025-06-06)
# 634'693 (2025-06-21)
# 634'422 (2025-07-22)
# 634'644 (2025-08-02)

# gridbase
#  49'247 (2019-05-26)
#  49'402 (2019-05-30)
#  54'314 (2019-07-31)
#  56'759 (2019-09-17)
#  58'656 (2019-10-26)
#  30'320 (2020-03-16)
#  31'787 (2020-04-10)
#  32'478 (2020-04-24)
#  34'203 (2020-06-01)
#  35'953 (2020-07-06)
#  37'038 (2020-07-28)
#  39'791 (2020-09-21)
#  43'435 (2020-12-03)
#  31'698 (2021-04-02)
#  32'015 (2021-04-08)
#  32'736 (2021-04-23)
#  34'708 (2021-06-02)
#  34'854 (2022-04-07)
#  35'874 (2022-04-28)
#  35'937 (2022-04-29)
#  36'630 (2022-05-13)
#  36'059 (2023-04-06)
#  36'356 (2023-04-12)
#  36'454 (2023-04-14)
#  37'763 (2023-05-10)
#  39'670 (2023-06-16)
#  54'828 (2023-09-16)
#  97'923 (2024-04-09)
#  64'713 (2024-05-14)
#  82'163 (2024-08-12)
# 135'436 (2025-01-23)
#  83'823 (2025-03-10)
#  98'100 (2025-05-20)
# 101'298 (2025-06-03)
# 101'842 (2025-06-06)
# 105'099 (2025-06-21)
# 111'736 (2025-07-22)
# 114'146 (2025-08-02)

}
\seealso{
\code{\link[=createIndex]{createIndex()}}
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Oct 2019
}
\keyword{data}
\keyword{file}
\keyword{internal}
