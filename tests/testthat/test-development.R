context("Development notes of package (not real tests")

if(FALSE) {


# libcurl returning OS dependent results:

if(!requireNamespace("RCurl", quietly=TRUE)) install.packages("RCurl")
link <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate"
RCurl::getURL(link, ftp.use.epsv=TRUE, dirlistonly=TRUE)
Sys.info()['sysname']
.Platform$OS.type

library(curl)
readLines(curl(link, handle=new_handle(dirlistonly=TRUE, ftp_use_epsv=TRUE)))


wp <- curl::curl(link, handle=curl::new_handle(dirlistonly=TRUE, ftp_use_epsv=TRUE))
readLines(wp)

# Windows / windows:
w <- "climate\r\nphenology\r\nradiosondes\r\nclimate_urban\r\n"
# Linux / unix:
l <- "climate\nphenology\nradiosondes\nclimate_urban\n"
# Darwin? (Mac)
# SunOS? (Solaris)

# generic solution:
gsub("\r", "", strsplit(w, "\n")[[1]])
gsub("\r", "", strsplit(l, "\n")[[1]])






# readDWD meta = TRUE
# Development process and tests

# in october 2016, DWD slightly changed monthly/kl meta file column widths
# Here are alternative thoughs on how to automatize width detection


"
> spaces
  [1]   6  15  24  25  26  27  28  29  30
 [10]  31  32  33  34  38  39  40  41  42
 [19]  50  51  52  53  60  65  66  67  68
 [28]  69  70  71  72  73  74  75  76  77
 [37]  78  79  80  81  82  83  84  85  86
 [46]  87  88  89  90  91  92  93  94  95
 [55]  96  97  98  99 100 101 119 120 121
 [64] 122 123 124 125 126 127 128 129 130
 [73] 131 132 133 134 135 136 137 138 139
 [82] 140 141 142 143 144 145 146 147 148
 [91] 149 150 151 152 153 154 155 156 157
[100] 158 159 160 161 162 163 164 165 166
[109] 167 168 169 170 171 172 173 174 175
[118] 176 177 178 179 180 181 182 183 184
[127] 185 186 187 188 189 190 191 192 193
[136] 194 195 196 197 198 199 200


> sb
  [1]   1   2   3   4   5   6   7   8   9
 [10]  10  12  21  30  31  32  33  34  35
 [19]  36  37  38  39  40  41  45  46  47
 [28]  48  49  57  58  59  60  67  72  73
 [37]  74  75  76  77  78  79  80  81  82
 [46]  83  84  85  86  87  88  89  90  91
 [55]  92  93  94  95  96  97  98  99 100
 [64] 101 102 103 104 105 106 107 108 126
 [73] 127 128 129 130 131 132 133 134 135
 [82] 136 137 138 139 140 141 142 143 144
 [91] 145 146 147 148 149 150 151 152 153
[100] 154 155 156 157 158 159 160 161 162
[109] 163 164 165 166 167 168 169 170 171
[118] 172 173 174 175 176 177 178 179 180
[127] 181 182 183 184 185 186 187 188 189
[136] 190 191 192 193 194 195 196 197 198
[145] 199 200
"

#             .        .        ..                .          .      .                                        .                      .
#             6        15       24   -   34   38-42      50-53      60   65               -                101                 119-200
      a="00001 18910101 19860630           478     47.8413    8.8493 Aach                                     Baden-Württemberg                                                                                  "
b="          1 19370101 19860630            478     47.8413    8.8493 Aach                                     Baden-Württemberg                                                                           "
#  1   -   10 12       21       30    -   41   45-49      57-60      67    72            -                  108                 126-200
#             *        *        **                *           *     *                                         *                       *
sa <- unlist(gregexpr(" ", a)) # monthly more_precip historical
sb <- unlist(gregexpr(" ", b)) # daily   kl          historical
sa[which(diff(sa)!=1)]
sa[which(diff(sa)!=1)+1]
sb[which(diff(sb)!=1)]


# Check a couple different styles with:
mf <- selectDWD(res=c(rep("hourly",3), "monthly", "daily"), var=c("cloudiness","solar","sun","kl","kl"),
          time=c(rep("r",4), "h"), meta=TRUE, outvec=T, current=TRUE)

m <- dataDWD(mf)
lapply(m, head)

# Also removed from readDWD (see note on selectDWD id argument):

#                ID           VON         BIS        HOEHE    LAT       LONG      NAME     BUNDESLAND
#colClasses <- c("character", "integer", "integer", "numeric","numeric","numeric","factor","factor")
# some meta files have no leading zeros, so this package uses integer all the time. # colClasses=colClasses


}

