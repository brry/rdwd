#' Process data from the DWD CDC FTP Server
#' 
#' Read climate variables (column meta data) from zip folders downloaded with
#' [dataDWD()].
#' The metadata file `"Metadaten_Parameter.*txt"` in the zip folder `file`
#' is read, processed and returned as a data.frame.\cr
#' `file` can be a vector with several filenames.
#' 
#' @return data.frame of the desired dataset,
#'         or a named list of data.frames if length(file) > 1.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso [dataDWD()], [readDWD()], [`dwdparams`], [newColumnNames()]\cr
#'          [readMeta()] for complete  `Metadaten_Parameter` file.\cr
#'          [website use case](https://bookdown.org/brry/rdwd/use-case-get-all-hourly-rainfall-data-20142016.html#read-the-data)
#' @keywords file
#' @importFrom utils read.table unzip
#' @importFrom berryFunctions checkFile na9 twarning
#' @importFrom pbapply pblapply
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' # see dataDWD
#' 
#' @param file    Char (vector): name(s) of the file(s) downloaded with [dataDWD()],
#'                e.g. "~/DWDdata/tageswerte_KL_02575_akt.zip"
#' @param params  data.frame: Parameter explanations. DEFAULT: [`dwdparams`]
#' @param quiet   Suppress message about non-abbreviated parameters?
#'                DEFAULT: FALSE through [rdwdquiet()]
#' @param progbar Logical: present a progress bar with estimated remaining time?
#'                If missing and length(file)==1, progbar is internally set to FALSE.
#'                DEFAULT: TRUE
#' 
readVars <- function(
file,
params=dwdparams,
quiet=rdwdquiet(),
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
# read file:
read_with_encoding <- function(enc)
 try(read.table(f, na.strings=na9(), sep=";", header=TRUE, nrows=nr-1,
                stringsAsFactors=FALSE, fileEncoding=enc),              silent=TRUE) 
tab <- read_with_encoding("latin1")
if(inherits(tab,"try-error")) tab <- read_with_encoding("UTF-8")
if(inherits(tab,"try-error")) tab <- read_with_encoding(readr::guess_encoding(f)$encoding[1])
if(inherits(tab,"try-error")) 
 {
 twarning("readVars read.table failed for '", file[i], "/Metadaten_Parameter*.txt'")
 return(NULL)
 }
#
tab <- tab[,c("Parameter", "Parameterbeschreibung", "Einheit")]
tab <- unique(tab)
#
dupli <- duplicated(tab$Parameter)
if(any(dupli)) twarning("The following entries are duplicated: ", 
                       toString(unique(tab$Parameter[dupli])),
                       "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'",
                       skip=2)
rownames(tab) <- NULL
#
# Merge with short variable descriptions:
tab2 <- merge(params, tab, all.y=TRUE)
kurzna <- is.na(tab2$Kurz)
if(any(kurzna) && !quiet) twarning("The following entries are not",
                        " abbreviated yet: ", toString(tab2$Parameter[kurzna]),
                        "\nThis occurs in '", fn, "/Metadaten_Parameter*.txt'.",
                        "\nPlease inform berry-b@gmx.de so this can be included!\n",
                        skip=2)
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
 twarning("The number of determined ",
         "'Metadaten_Parameter*.txt' files should be 1, but is instead:\n",
         paste(msg[ exp],collapse="\n"), if(any( exp)) mexp,
         paste(msg[!exp],collapse="\n"), if(any(!exp)) mnexp)
 }
#
names(output) <- tools::file_path_sans_ext(basename(file))
output <- if(length(file)==1) output[[1]] else output
return(output)
}



# dwdparams --------------------------------------------------------------------

# updateIndexes will add new rows with NA for Kurz in 'misc/params.txt'
# Copy that to 'misc/params.xlsx', sheet 'input'.
# Add the Kurz entries there manually.
# Copy the 'output' sheet below here.
# Remove the 0 0 row at the end.

#' @title DWD parameter explanations
#' @description Short German parameter explanations for the DWD abbreviations
#' on the CDC FTP server.\cr
#' These are manually created by me and might need to be expanded if the DWD adds
#' more abbreviations.\cr
#' [readVars()] maps them to the variable abbreviations in the
#' `"Metadaten_Parameter.*txt"` file in any given zip folder
#' and will warn about missing entries.
#' 
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jun 2018
#' @seealso [readVars()], [readDWD()]
#' @keywords datasets
#' @export
#' @examples
#' head(dwdparams)
#' 
dwdparams <- unique(read.table(header=TRUE, strip.white=TRUE,
                               stringsAsFactors=FALSE, text="
Parameter	Kurz
ABSF_STD	Absolute_Feuchte
JA_N	Bedeckungsgrad
MO_N	Bedeckungsgrad
N_TER	Bedeckungsgrad
NM	Bedeckungsgrad
V_N	Bedeckungsgrad
V_S1_NS	Bedeckungsgrad_Schicht1
V_S2_NS	Bedeckungsgrad_Schicht2
V_S3_NS	Bedeckungsgrad_Schicht3
V_S4_NS	Bedeckungsgrad_Schicht4
WW	Beobachtung
VP_STD	Dampfdruck
VP_TER	Dampfdruck
VPM	Dampfdruck
E_TF_TER	Eisansatz
V_TE002	Erdbodentemperatur_002cm
V_TE002M	Erdbodentemperatur_002cm
V_TE005	Erdbodentemperatur_005cm
V_TE005M	Erdbodentemperatur_005cm
V_TE010	Erdbodentemperatur_010cm
V_TE010M	Erdbodentemperatur_010cm
V_TE020	Erdbodentemperatur_020cm
V_TE020M	Erdbodentemperatur_020cm
V_TE050	Erdbodentemperatur_050cm
V_TE050M	Erdbodentemperatur_050cm
V_TE100	Erdbodentemperatur_100cm
EK_TER	Erdbodenzustand
TF_STD	Feuchttemperatur
TF_TER	Feuchttemperatur
GEWITTER	Gewitter
JA_GEWITTER	Gewitter
MO_GEWITTER	Gewitter
RR_GEWITTER	Gewitter
GLATTEIS	Glatteis
JA_GLATTEIS	Glatteis
MO_GLATTEIS	Glatteis
GRAUPEL	Graupel
JA_GRAUPEL	Graupel
MO_GRAUPEL	Graupel
RR_GRAUPEL	Graupel
HAGEL	Hagel
JA_HAGEL	Hagel
MO_HAGEL	Hagel
RR_HAGEL	Hagel
P_STD	Luftdruck
PM	Luftdruck
P	Luftdruck_NN
P0	Luftdruck_Stationshoehe
PP_TER	Luftdruck_Terminwert
JA_TT	Lufttemperatur
MO_TT	Lufttemperatur
TMK	Lufttemperatur
TT	Lufttemperatur
TT_STD	Lufttemperatur
TT_TU	Lufttemperatur
TGK	Lufttemperatur_5cm_min
JA_MX_TX	Lufttemperatur_AbsMax
MX_TX	Lufttemperatur_AbsMax
JA_MX_TN	Lufttemperatur_AbsMin
MX_TN	Lufttemperatur_AbsMin
JA_TX	Lufttemperatur_Max
MO_TX	Lufttemperatur_Max
TXK	Lufttemperatur_Max
JA_TN	Lufttemperatur_Min
MO_TN	Lufttemperatur_Min
TNK	Lufttemperatur_Min
TT_TER	Lufttemperatur_Terminwert
JA_NEBEL	Nebel
MO_NEBEL	Nebel
NEBEL	Nebel
RR_NEBEL	Nebel
RSF	Niederschlagsform
RSKF	Niederschlagsform
WRTR	Niederschlagsform
JA_RR	Niederschlagshoehe
MO_RR	Niederschlagshoehe
R1	Niederschlagshoehe
RS	Niederschlagshoehe
RSK	Niederschlagshoehe
JA_MX_RS	Niederschlagshoehe_Max
MX_RS	Niederschlagshoehe_Max
RS_IND	Niederschlagsindikator
REIF	Reif
RF_STD	Relative_Feuchte
RF_TU	Relative_Feuchte
UPM	Relative_Feuchte
RF_TER	Relative_Feuchte_Terminwert
JA_SH_S	Schneehoehe
MO_SH_S	Schneehoehe
SH_TAG	Schneehoehe
SHK_TAG	Schneehoehe
ASH_6	Schneehoehe_Ausstich
JA_NSH	Schneehoehe_Neu
MO_NSH	Schneehoehe_Neu
NSH_TAG	Schneehoehe_Neu
WAAS_6	Schneewasseraequivalent
WASH_6	Schneewasseraequivalent_Gesamt
V_VV	Sichtweite
VK_TER	Sichtweite
JA_SD_S	Sonnenscheindauer
MO_SD_S	Sonnenscheindauer
SD_LBERG	Sonnenscheindauer
SD_SO	Sonnenscheindauer
SD_STRAHL	Sonnenscheindauer
SDK	Sonnenscheindauer
ATMO_LBERG	Strahlung_Atmospaere
ATMO_STRAHL	Strahlung_Atmospaere
FG_LBERG	Strahlung_Global_kurzwellig
FG_STRAHL	Strahlung_Global_kurzwellig
FD_LBERG	Strahlung_Himmel_diffus
FD_STRAHL	Strahlung_Himmel_diffus
JA_STURM_6	Sturm_6Bft
MO_STURM_6	Sturm_6Bft
STURM_6	Sturm_6Bft
JA_STURM_8	Sturm_8Bft
MO_STURM_8	Sturm_8Bft
STURM_8	Sturm_8Bft
JA_TAU	Tau
MO_TAU	Tau
TAU	Tau
TD	Taupunkttemperatur
TD_STD	Taupunkttemperatur
F	Windgeschwindigkeit
FF	Windgeschwindigkeit
FM	Windgeschwindigkeit
D	Windrichtung
DD	Windrichtung
DK_TER	Windrichtung
FX	Windspitze
JA_MX_FX	Windspitze
MX_FX	Windspitze
FX_911	Windspitze_Stunde1
FX_911_3	Windspitze_Stunde3
FX_911_6	Windspitze_Stunde6
FK_TER	Windstaerke
JA_FK	Windstaerke
MO_FK	Windstaerke
V_S1_CSA	Wolkenart_Abk_Schicht1
V_S2_CSA	Wolkenart_Abk_Schicht2
V_S3_CSA	Wolkenart_Abk_Schicht3
V_S4_CSA	Wolkenart_Abk_Schicht4
V_S1_CS	Wolkenart_Schicht1
V_S2_CS	Wolkenart_Schicht2
V_S3_CS	Wolkenart_Schicht3
V_S4_CS	Wolkenart_Schicht4
CD_TER	Wolkendichte
V_S1_HHS	Wolkenhoehe_Schicht1
V_S2_HHS	Wolkenhoehe_Schicht2
V_S3_HHS	Wolkenhoehe_Schicht3
V_S4_HHS	Wolkenhoehe_Schicht4
JA_EISTAGE	Anzahl_Eistage
JA_FROSTTAGE	Anzahl_Frosttage
JA_HEISSE_TAGE	Anzahl_Heisse_Tage
JA_RR_GE_0_1_MM	Anzahl_Niederschlag00.1
JA_RR_GE_1_0_MM	Anzahl_Niederschlag01
JA_RR_GE_10_0_MM	Anzahl_Niederschlag10
JA_RR_GE_2_5_MM	Anzahl_Niederschlag02.5
JA_RR_GE_20_0_MM	Anzahl_Niederschlag20
JA_RR_GE_5_0_MM	Anzahl_Niederschlag05
JA_SH_GE_1_0_CM	Anzahl_Schnee1
JA_SH_GE_5_0_CM	Anzahl_Schnee5
JA_SOMMERTAGE	Anzahl_Sommertage
JA_TROPENNAECHTE	Anzahl_Tropennaechte
MO_EISTAGE	Anzahl_Eistage
MO_FROSTTAGE	Anzahl_Frosttage
MO_HEISSE_TAGE	Anzahl_Heisse_Tage
MO_RR_GE_0_1_MM	Anzahl_Niederschlag00.1
MO_RR_GE_1_0_MM	Anzahl_Niederschlag01
MO_RR_GE_10_0_MM	Anzahl_Niederschlag10
MO_RR_GE_2_5_MM	Anzahl_Niederschlag02.5
MO_RR_GE_20_0_MM	Anzahl_Niederschlag20
MO_RR_GE_5_0_MM	Anzahl_Niederschlag05
MO_SH_GE_1_0_CM	Anzahl_Schnee1
MO_SH_GE_5_0_CM	Anzahl_Schnee5
MO_SOMMERTAGE	Anzahl_Sommertage
MO_TROPENNAECHTE	Anzahl_Tropennaechte
"))
rownames(dwdparams) <- dwdparams$Parameter
