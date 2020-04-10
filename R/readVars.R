#' Process data from the DWD CDC FTP Server
#' 
#' Read climate variables (column meta data) from zip folders downloaded with 
#' \code{\link{dataDWD}}.
#' The metadata file \code{"Metadaten_Parameter.*txt"} in the zip folder \code{file} 
#' is read, processed and returned as a data.frame.\cr
#' \code{file} can be a vector with several filenames. 
#' 
#' @return data.frame of the desired dataset, 
#'         or a named list of data.frames if length(file) > 1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso \code{\link{dataDWD}}, \code{\link{readDWD}}, \code{\link{dwdparams}}
#' @keywords file
#' @importFrom utils read.table unzip
#' @importFrom berryFunctions checkFile na9 traceCall
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD
#' 
#' @param file   Char (vector): name(s) of the file(s) downloaded with \code{\link{dataDWD}},
#'               e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip" 
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'               If missing and length(file)==1, progbar is internally set to FALSE.
#'               DEFAULT: TRUE
#' 
readVars <- function(
file,
progbar=TRUE
)
{
# Optional progress bar:
if(missing(progbar) & length(file)==1) progbar <- FALSE
if(progbar) lapply <- pbapply::pblapply
#
checkFile(file)
#
# loop over each filename
output <- lapply(seq_along(file), function(i)
{
f <- file[i]
# temporary unzipping directory
fn <- tools::file_path_sans_ext(basename(f))
exdir <- paste0(tempdir(),"/", fn)
unzip(f, exdir=exdir)
on.exit(unlink(exdir, recursive=TRUE), add=TRUE)
f <- dir(exdir, pattern="Metadaten_Parameter.*txt", full.names=TRUE)
if(length(f)!=1) return(length(f))
nr <- readLines(f) # number of rows
nr <- sum(!substr(nr, 1, 7) %in% c("Legende", "generie"))
tab <- read.table(f, na.strings=na9(), sep=";", header=TRUE, nrows=nr-1, 
                  stringsAsFactors=FALSE)
#
tab <- tab[,c("Parameter", "Parameterbeschreibung", "Einheit")]
tab <- unique(tab)
#
dupli <- duplicated(tab$Parameter)
if(any(dupli)) warning(traceCall(3, "", ": "), "The following entries are",
                       " duplicated: ", toString(unique(tab$Parameter[dupli])),
                       "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'",
                       call.=FALSE)
rownames(tab) <- NULL
#
# Merge with short variable descriptions:
tab2 <- merge(dwdparams, tab, all.y=TRUE)
kurzna <- is.na(tab2$Kurz)
if(any(kurzna)) warning(traceCall(3, "", ": "), "The following entries are not",
                        " abbreviated yet: ", toString(tab2$Parameter[kurzna]),
                        "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'.",
                        "\nPlease inform berry-b@gmx.de so this can be included!\n",
                        call.=FALSE)
#
colnames(tab2)[1] <- "Par"
rownames(tab2) <- tab2$Par
# return column metadata:
return(tab2)
# lapply loop end
})
#
# Warn about zip folders with no meta file:
nometa <- sapply(output, class)=="integer"
if(any(nometa)) 
 {
 msg <- paste(unlist(output[nometa]), file[nometa], sep=" in ")
 exp <- grepl("_minute", file[nometa]) # expected no meta files
 mexp <- c("\nThis is expected since 1 and 10 minute data do not have ",
           "meta-information in most of the zip folders (as of 2019-02).\n")
 mnexp <- "\nPlease contact berry-b@gmx.de with with a copy of this warning.\n"
 warning(traceCall(1, "", ": "), "The number of determined ",
         "'Metadaten_Parameter*.txt' files should be 1, but is instead:\n", 
         paste(msg[ exp],collapse="\n"), if(any( exp)) mexp, 
         paste(msg[!exp],collapse="\n"), if(any(!exp)) mnexp,
         call.=FALSE)
 }
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(output)
}



# dwdparams --------------------------------------------------------------------

#' @title DWD parameter explanations
#' @description Short German parameter explanations for the DWD abbreviations
#' on the CDC FTP server.\cr
#' These are manually created by me and might need to be expanded if the DWD adds
#' more abbreviations.\cr
#' \code{\link{readVars}} maps them to the variable abbreviations in the
#' \code{"Metadaten_Parameter.*txt"} file in any given zip folder
#' and will warn about missing entries.
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso \code{\link{readVars}}, \code{\link{readDWD}}
#' @keywords datasets
#' @export
#' @examples
#' head(dwdparams)
#' 
dwdparams <- unique(read.table(header=TRUE, strip.white=TRUE,
                               stringsAsFactors=FALSE, text="
Parameter	Kurz
JA_N	Bedeckungsgrad
JA_N	Bedeckungsgrad
MO_N	Bedeckungsgrad
MO_N	Bedeckungsgrad
N_TER	Bedeckungsgrad
N_TER	Bedeckungsgrad
NM	Bedeckungsgrad
NM	Bedeckungsgrad
V_N	Bedeckungsgrad
V_N	Bedeckungsgrad
V_N	Bedeckungsgrad
V_N	Bedeckungsgrad
V_S1_NS	Bedeckungsgrad_Schicht1
V_S1_NS	Bedeckungsgrad_Schicht1
V_S2_NS	Bedeckungsgrad_Schicht2
V_S2_NS	Bedeckungsgrad_Schicht2
V_S3_NS	Bedeckungsgrad_Schicht3
V_S3_NS	Bedeckungsgrad_Schicht3
V_S4_NS	Bedeckungsgrad_Schicht4
V_S4_NS	Bedeckungsgrad_Schicht4
VP_TER	Dampfdruck
VP_TER	Dampfdruck
VPM	Dampfdruck
VPM	Dampfdruck
E_TF_TER	Eisansatz
E_TF_TER	Eisansatz
V_TE002	Erdbodentemperatur_002cm
V_TE002	Erdbodentemperatur_002cm
V_TE002M	Erdbodentemperatur_002cm
V_TE002M	Erdbodentemperatur_002cm
V_TE005	Erdbodentemperatur_005cm
V_TE005	Erdbodentemperatur_005cm
V_TE005M	Erdbodentemperatur_005cm
V_TE005M	Erdbodentemperatur_005cm
V_TE010	Erdbodentemperatur_010cm
V_TE010	Erdbodentemperatur_010cm
V_TE010M	Erdbodentemperatur_010cm
V_TE010M	Erdbodentemperatur_010cm
V_TE020	Erdbodentemperatur_020cm
V_TE020	Erdbodentemperatur_020cm
V_TE020M	Erdbodentemperatur_020cm
V_TE020M	Erdbodentemperatur_020cm
V_TE050	Erdbodentemperatur_050cm
V_TE050	Erdbodentemperatur_050cm
V_TE050M	Erdbodentemperatur_050cm
V_TE050M	Erdbodentemperatur_050cm
V_TE100	Erdbodentemperatur_100cm
V_TE100	Erdbodentemperatur_100cm
EK_TER	Erdbodenzustand
EK_TER	Erdbodenzustand
TF_TER	Feuchttemperatur
TF_TER	Feuchttemperatur
PM	Luftdruck
PM	Luftdruck
PP_10	Luftdruck
P	Luftdruck_NN
P	Luftdruck_NN
P0	Luftdruck_Stationshoehe
P0	Luftdruck_Stationshoehe
PP_TER	Luftdruck_Terminwert
PP_TER	Luftdruck_Terminwert
JA_TT	Lufttemperatur
JA_TT	Lufttemperatur
MO_TT	Lufttemperatur
MO_TT	Lufttemperatur
TMK	Lufttemperatur
TMK	Lufttemperatur
TT	Lufttemperatur
TT	Lufttemperatur
TT_10	Lufttemperatur
TT_TU	Lufttemperatur
TT_TU	Lufttemperatur
TM5_10	Lufttemperatur_5cm
TX5_10	Lufttemperatur_5cm_max
TGK	Lufttemperatur_5cm_min
TGK	Lufttemperatur_5cm_min
TN5_10	Lufttemperatur_5cm_min
JA_MX_TX	Lufttemperatur_AbsMax
JA_MX_TX	Lufttemperatur_AbsMax
MX_TX	Lufttemperatur_AbsMax
MX_TX	Lufttemperatur_AbsMax
JA_MX_TN	Lufttemperatur_AbsMin
JA_MX_TN	Lufttemperatur_AbsMin
MX_TN	Lufttemperatur_AbsMin
MX_TN	Lufttemperatur_AbsMin
JA_TX	Lufttemperatur_Max
JA_TX	Lufttemperatur_Max
MO_TX	Lufttemperatur_Max
MO_TX	Lufttemperatur_Max
TX_10	Lufttemperatur_Max
TXK	Lufttemperatur_Max
TXK	Lufttemperatur_Max
JA_TN	Lufttemperatur_Min
JA_TN	Lufttemperatur_Min
MO_TN	Lufttemperatur_Min
MO_TN	Lufttemperatur_Min
TN_10	Lufttemperatur_Min
TNK	Lufttemperatur_Min
TNK	Lufttemperatur_Min
TT_TER	Lufttemperatur_Terminwert
TT_TER	Lufttemperatur_Terminwert
RWS_DAU_10	Niederschlagsdauer
RSF	Niederschlagsform
RSF	Niederschlagsform
RSKF	Niederschlagsform
RSKF	Niederschlagsform
WRTR	Niederschlagsform
WRTR	Niederschlagsform
JA_RR	Niederschlagshoehe
JA_RR	Niederschlagshoehe
JA_RR	Niederschlagshoehe
JA_RR	Niederschlagshoehe
MO_RR	Niederschlagshoehe
MO_RR	Niederschlagshoehe
MO_RR	Niederschlagshoehe
MO_RR	Niederschlagshoehe
R1	Niederschlagshoehe
R1	Niederschlagshoehe
RS	Niederschlagshoehe
RS	Niederschlagshoehe
RS_01	Niederschlagshoehe
RSK	Niederschlagshoehe
RSK	Niederschlagshoehe
RWS_10	Niederschlagshoehe
JA_MX_RS	Niederschlagshoehe_Max
JA_MX_RS	Niederschlagshoehe_Max
JA_MX_RS	Niederschlagshoehe_Max
JA_MX_RS	Niederschlagshoehe_Max
MX_RS	Niederschlagshoehe_Max
MX_RS	Niederschlagshoehe_Max
MX_RS	Niederschlagshoehe_Max
MX_RS	Niederschlagshoehe_Max
RTH_01	Niederschlagshoehe_Tropfen
RWH_01	Niederschlagshoehe_Wippe
RS_IND	Niederschlagsindikator
RS_IND	Niederschlagsindikator
RS_IND_01	Niederschlagsindikator
RWS_IND_10	Niederschlagsindikator
RF_10	Relative_Feuchte
RF_TU	Relative_Feuchte
RF_TU	Relative_Feuchte
UPM	Relative_Feuchte
UPM	Relative_Feuchte
RF_TER	Relative_Feuchte_Terminwert
RF_TER	Relative_Feuchte_Terminwert
RF_TER	Relative_Feuchte_Terminwert
RF_TER	Relative_Feuchte_Terminwert
JA_SH_S	Schneehoehe
JA_SH_S	Schneehoehe
MO_SH_S	Schneehoehe
MO_SH_S	Schneehoehe
SH_TAG	Schneehoehe
SH_TAG	Schneehoehe
SH_TAG	Schneehoehe
SH_TAG	Schneehoehe
SHK_TAG	Schneehoehe
SHK_TAG	Schneehoehe
ASH_6	Schneehoehe_Ausstich
ASH_6	Schneehoehe_Ausstich
JA_NSH	Schneehoehe_Neu
JA_NSH	Schneehoehe_Neu
MO_NSH	Schneehoehe_Neu
MO_NSH	Schneehoehe_Neu
NSH_TAG	Schneehoehe_Neu
NSH_TAG	Schneehoehe_Neu
WAAS_6	Schneewasseraequivalent
WAAS_6	Schneewasseraequivalent
WASH_6	Schneewasseraequivalent_Gesamt
WASH_6	Schneewasseraequivalent_Gesamt
V_VV	Sichtweite
V_VV	Sichtweite
VK_TER	Sichtweite
VK_TER	Sichtweite
JA_SD_S	Sonnenscheindauer
JA_SD_S	Sonnenscheindauer
MO_SD_S	Sonnenscheindauer
MO_SD_S	Sonnenscheindauer
SD_10	Sonnenscheindauer
SD_LBERG	Sonnenscheindauer
SD_SO	Sonnenscheindauer
SD_SO	Sonnenscheindauer
SD_STRAHL	Sonnenscheindauer
SDK	Sonnenscheindauer
SDK	Sonnenscheindauer
ATMO_LBERG	Strahlung_Atmospaere
ATMO_STRAHL	Strahlung_Atmospaere
GS_10	Strahlung_Global
FG_LBERG	Strahlung_Global_kurzwellig
FG_STRAHL	Strahlung_Global_kurzwellig
DS_10	Strahlung_Himmel_diffus
FD_LBERG	Strahlung_Himmel_diffus
FD_STRAHL	Strahlung_Himmel_diffus
LS_10	Strahlung_langwellig
TD	Taupunkttemperatur
TD	Taupunkttemperatur
TD_10	Taupunkttemperatur
F	Windgeschwindigkeit
F	Windgeschwindigkeit
FF	Windgeschwindigkeit
FF	Windgeschwindigkeit
FF_10	Windgeschwindigkeit
FM	Windgeschwindigkeit
FM	Windgeschwindigkeit
FX_10	Windgeschwindigkeit_Max
FMX_10	Windgeschwindigkeit_MaxMean
FNX_10	Windgeschwindigkeit_Min
D	Windrichtung
DD	Windrichtung
DD_10	Windrichtung
DK_TER	Windrichtung
DK_TER	Windrichtung
DX_10	Windrichtung_Maxwind
FX	Windspitze
FX	Windspitze
JA_MX_FX	Windspitze
JA_MX_FX	Windspitze
MX_FX	Windspitze
MX_FX	Windspitze
FK_TER	Windstaerke
FK_TER	Windstaerke
JA_FK	Windstaerke
JA_FK	Windstaerke
MO_FK	Windstaerke
MO_FK	Windstaerke
V_S1_CSA	Wolkenart_Abk_Schicht1
V_S1_CSA	Wolkenart_Abk_Schicht1
V_S2_CSA	Wolkenart_Abk_Schicht2
V_S2_CSA	Wolkenart_Abk_Schicht2
V_S3_CSA	Wolkenart_Abk_Schicht3
V_S3_CSA	Wolkenart_Abk_Schicht3
V_S4_CSA	Wolkenart_Abk_Schicht4
V_S4_CSA	Wolkenart_Abk_Schicht4
V_S1_CS	Wolkenart_Schicht1
V_S1_CS	Wolkenart_Schicht1
V_S2_CS	Wolkenart_Schicht2
V_S2_CS	Wolkenart_Schicht2
V_S3_CS	Wolkenart_Schicht3
V_S3_CS	Wolkenart_Schicht3
V_S4_CS	Wolkenart_Schicht4
V_S4_CS	Wolkenart_Schicht4
CD_TER	Wolkendichte
CD_TER	Wolkendichte
V_S1_HHS	Wolkenhoehe_Schicht1
V_S1_HHS	Wolkenhoehe_Schicht1
V_S2_HHS	Wolkenhoehe_Schicht2
V_S2_HHS	Wolkenhoehe_Schicht2
V_S3_HHS	Wolkenhoehe_Schicht3
V_S3_HHS	Wolkenhoehe_Schicht3
V_S4_HHS	Wolkenhoehe_Schicht4
V_S4_HHS	Wolkenhoehe_Schicht4
GEWITTER	Gewitter
GEWITTER	Gewitter
GLATTEIS	Glatteis
GLATTEIS	Glatteis
GRAUPEL	Graupel
GRAUPEL	Graupel
HAGEL	Hagel
HAGEL	Hagel
JA_GEWITTER	Gewitter
JA_GEWITTER	Gewitter
JA_GLATTEIS	Glatteis
JA_GLATTEIS	Glatteis
JA_GRAUPEL	Graupel
JA_GRAUPEL	Graupel
JA_HAGEL	Hagel
JA_HAGEL	Hagel
JA_NEBEL	Nebel
JA_NEBEL	Nebel
JA_STURM_6	Sturm_6Bft
JA_STURM_6	Sturm_6Bft
JA_STURM_8	Sturm_8Bft
JA_STURM_8	Sturm_8Bft
JA_TAU	Tau
JA_TAU	Tau
MO_GEWITTER	Gewitter
MO_GEWITTER	Gewitter
MO_GLATTEIS	Glatteis
MO_GLATTEIS	Glatteis
MO_GRAUPEL	Graupel
MO_GRAUPEL	Graupel
MO_HAGEL	Hagel
MO_HAGEL	Hagel
MO_NEBEL	Nebel
MO_NEBEL	Nebel
MO_STURM_6	Sturm_6Bft
MO_STURM_6	Sturm_6Bft
MO_STURM_8	Sturm_8Bft
MO_STURM_8	Sturm_8Bft
MO_TAU	Tau
MO_TAU	Tau
NEBEL	Nebel
NEBEL	Nebel
REIF	Reif
REIF	Reif
STURM_6	Sturm_6Bft
STURM_6	Sturm_6Bft
STURM_8	Sturm_8Bft
STURM_8	Sturm_8Bft
TAU	Tau
TAU	Tau
SLA_10	Windgeschwindigkeit_STABW_lat
SLO_10	Windgeschwindigkeit_STABW_lon
"))
rownames(dwdparams) <- dwdparams$Parameter
