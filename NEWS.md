
# Version 1.9.3 (2025-08-18)

- new functions: `readDWD.hyras`, `readDWD.asczip`, `checkUpdates`
- `dataDWD`: fileIndex warning now also visible for long vectors
- `selectDWD`: handles files removed from FTP but still in index
- `readDWD.multia`: new argument tryenc to try different encodings
- `readDWD.data`: convert timestamp to as.Date
- `readDWD.meta`: reflect DWD changes (Abgabe column, standard_format beschreibung file)
- `readDWD.nc`: flip the map upright 
- `indexFTP`: more options and checks, reflect removal of tree.html 
- `nearbyStations`: new arguments mindex and current
- `checkIndex`: checks + messages improved, false positives excluded
- documentation + website improved in several spots
- rhub tests updated

# Version 1.8.0 (2023-06-17)

- `readDWD.grib2`: pack option rgdal removed.
- all instances of `raster`/`rgdal`/`sp` code replaced with `terra` code
- cross references and website improved

# Version 1.7.0 (2023-04-14)

- `dataDWD`: *dir* default set to `locdir()`
- interactive weather comparison app added
- `findID`+`selectDWD`: new argument *failempty* to stop instead of warn if name does not match
- `readDWD.grib2`: rgdal retirement warning added
- `dataDWD` + `readDWD`: new argument *hr* to auto-merge historical and recent files
- `createIndex`: speed and correctness improved
- `runLocalTests`: tests expanded, internally restructured
- `readDWD`: new argument *quietread*
- this NEWS file is completely reformatted and changed into NEWS.md


# Version 1.6.0 (2022-05-30)

## new functions
- `plotDWD`
- `locdir` (will replace fixed "DWDdata" folder in `dataDWD`)
- `readDWD.deriv`
- `readDWD.rklim`
- `readDWD.pdf`
- `validFileTypes` (charstring vector)
## improvements
- `selectDWD`: is completely rewritten and now vectorizes expandingly
- reading functions: use latin1 encoding
- `dataDWD`: new argument *method*, new default for *overwrite*
- several improvements for input checks, messages, tests and documentation
- `metaInfo`: accepts custom index
- `readDWD.radar`: new argument *dividebyten* 
- `readDWD.grib2`: new argument *pack* 
- website: new use case daily radar files


# Version 1.5.0 (2021-04-08)

## API changes
- `readVars`: new argument *quiet* (for updateIndexes)
- `readDWD.grib2`: initial release
- `projectRasterDWD`: new argument *adjust05* 
- `dataDWD`: new default: *dbin*=TRUE
## enhancements / bug fixes
- `readDWD.data`: fread checks for system command `unzip`, prints final value in message
- `dataDWD`: unfound URL messages also link to website fileIndex in German locale
- `updateRdwd`: version and date comparison improved, print local version if newer than on github
- `createIndex`: improved recognition of "Beschreibung_Stationen" meta files
- `dwdparams` + `updateIndexes`: missing/duplicate 'Kurz' entry messages improved
- `readDWD`: subfunction names in elegant message outside the loop to avoid interrupted progbars.
- `indexFTP` + `dirDWD`: trailing slashes removed with regexp
- `fileType`: more informative message for failed type determination 
- `DEU`: now derived from NUTS regions and with new CRS (along with `EUR`)
- `runLocalTests`: improved logfiles, false positive messages removed 
## website changes
- package structure moved to first chapter
- fread instructions expanded
- FTP Folder list expanded
- more messages / warnings suppressed that are irrelevant to readers
- linked to in seeAlso sections of suitable function documentations
- redirected man pages are now correctly linked to (e.g. fileIndex -> index)
- documentation and dataset links are now opened in a new window
- new use case: values at locations in grid
- FTP address updated everywhere


# Version 1.4.0 (2020-07-31)

## API changes
- `dataDWD`: argument *file* renamed to *url* to avoid confusion with local file names
- `readDWD`: *fread* default set to NA. This is an experiment, see issue #22 
- `readDWD`: now has argument *type* (determined by new function `fileType`) to replace the ever growing list of types
- `dataDWD`, `readDWD`, `selectDWD`: order of arguments changed to reflect importance
- `plotRadar`: gains arguments *axes*, *las*, *zlim*, *col*, *mar*, *keeppar*. *main* now defaults to (and correctly keeps) x@title.
- `selectDWD`: gains argument *remove_dupli* to ignore DWD file upload errors
- `readDWD.*`: All subfunctions message their identity and have a *quiet* argument
- new function: `rdwdquiet`()
- `readDWD.meta`: station and bundesland names are now char instead of factor (since R 4.0.0)
- `indexFTP`: new argument *fast* to read file tree with data.table
- `createIndex`: gains argument *checklog*
- `runLocalTests`: new arguments for `devtools::check` and `checkIndex`
- `readVars`: *params* now an explicit argument (defaults to `dwdparams`)
## enhancements / bug fixes
- `EUR`: extends further east for plots with large width compared to height
- `updateRdwd`: unloads package before installation and informs about the need to re-load
- `checkIndex`: new check for duplicate meta files, writes to logfile (append, no overwrite), nicer path print, logs modification time of index files
- `lldist`: fix error that occurred when a df with a single row was given
- `runLocalTests`: warning logfile cleared before writing, `checkIndex` results included, false positives removed, on github
- source code function argument sections all have line breaks (except `readDWD.*`)
- documentation source code now is written in markdown, source files have been reorganized
- cross-references in documentation improved
- `metaInfo`: from/to columns date conversion corrected
- `indexFTP` + `dataDWD`: up to date and centralized sleep information 
- `dataDWD`: Download failure message improved, refer to fileIndex section on homepage if applicable.
- `readDWD.nc`: ncdf warning suppressed
- `selectDWD`: combines all warnings within the loop to a single message
## website changes
- improved (and stand-alone) fileIndex section
- `plotRadar` used for nice maps
- huxtable fixed
- use global *quiet* option instead of explicit `quiet=TRUE` all the time
- history section added
- package schematic revamped completely


# Version 1.3.1 (2020-02-18)

## API changes
- `projectRasterDWD`: *latlon*=T/F replaced with *targetproj*
- `readDWD.binary`: output list element renamed to 'dat' (from 'data') for consistency with other functions
- `dataDWD`: new argument *dbin*
- new functions: `updateRdwd`, `plotRadar`
## enhancements / bug fixes
- `readDWD.data`: now correctly reads 10 minute timestamps
- `readDWD.meta`: column widths identified more elegantly and safely
- `indexFTP`: warn about requests to https instead of ftp servers, new argument `exclude.latest.bin`
- `addBorders`: added sp to 'Suggests', moved `DEU` + `EUR` to inst/extdata to handle CRAN build+check on systems without sp installed
## website changes
- list of FTP Folders now with clickable URLs
- RQ example added
- order of radar graphs changed for better comparison in pdf


# Version 1.2.0 (2019-10-26)

## Highlights
- Vignettes moved to <https://bookdown.org/brry/rdwd> with nice structure
- improved reading of gridded data, documented in vignette chapter "Raster data"
- `readRadarFile` expanded + moved to own package (**dwdradar**) with proper tests and better warnings
- Hexsticker created
## Important changes
- `readDWD.binary`: untarring improved
- `projectRasterDWD`: default extents + projs added for seasonal + nc
- source code directory structure updated
- *quiet* argument added to many functions
- `readDWD`: order and documentation of method selection improved
- `metaIndex` + `fileIndex`: date columns are now of class Date
- `dataDWD`: *force* can now be number of hours after which to re-download
- various minor fixes, see <https://github.com/brry/rdwd/commits>
## New functions + datasets
- `readDWD.radar` + `readDWD.nc` + `readDWD.stand`
- `checkSuggestedPackages`
- `EUR` + `addBorders`
- `formatIndex`
- `runLocalTests`
- `updateIndexes` (not exported)


# Version 1.1.0 (2019-05-30)

## Highlights:
- Binary Radolan data can now be read correctly
- The new FTP server is used
- Raster data can easily be projected
- All `readDWD` subfunctions now have their own documentation
## Important changes:
- `dataDWD`: arguments to `readDWD` removed (can be passed with ...)
- `dataDWD`: gains *joinbf* argument to join *base* and relative *url* path
- `readDWD`: can expand DWD abbreviations in column names (*varnames*=TRUE)
- `readDWD`: source code structure is improved.
- `readDWD.multia`: ignores EndOfFile characters in multiannual data on Unix
- `readDWD.raster`: accepts `gunzip` arguments, reads faster on second calls
- `readDWD.data`: informatively handles empty files
- `readDWD.binary+raster+asc`: overlay Germany maps in example plots
- `readDWD.binary`: unzips into a given *exdir* (and reads faster on second calls)
- `readDWD.binary`: transforms output to a raster stack
- `selectDWD`: suggests only available data in interactive option
- `selectDWD` and `nearbyStations`: fail informatively for typical errors
- `fileIndex`: loses the unnnecessary leading slashes
- `indexFTP`: uses tree of files at new FTP server
## New user-visible functions + objects: 
- `newColumnNames`
- `dwdparams` (renamed from `parameter_abreviations`)
- `gridbase`
- `gridIndex`
- `projectRasterDWD`
- `localtestdir` (though mainly for internal usage)
## New unexported functions:
- `readDWD.asc`
- `readRadarfile` + `bin2num` + FORTRAN code
- `checkIndex`


# Version 1.0.0 (2019-03-17)

First major version increase!

- Tests are now systematic and comprehensive (albeit run locally and manually to reduce CRAN load and enable local file writing).
- `selectDWD` got a very noticable performance boost and an interactive *res/var/per* selection.
- Handling raster files is now supported in rdwd.
- Examples and vignettes have been refined.
- Detailed changes can be found on <https://github.com/brry/rdwd/commits>
## main overview
- `dataDWD`: *dots* now passed to `readDWD`, download errors checked and returned as informative warning
- `readDWD`: reading functions added for binary, raster and multi_annual files, MESS_DATUM POSIXct column added in monthly data, list output now named
- `indexFTP`: now removes duplicates from file list, sped up in default 1min/prec usage
- rdwd now works fine with multi_annual and subdaily data
- `readVars`: parameter abbreviations completed, output now visible
- `selectDWD`: order of arguments changed, interactive *res/var/per* selection enabled, computing speed extremely increased
- `localtests.R` heavily expanded, examples reduced and refined
- indexes + vignettes updated, use cases expanded with `nearbyStations` example
- New functions and objects: `readMeta`, `dwdbase` (central package base url)


# Version 0.11.0 (2018-11-26)

- `dataDWD`: argument *force* can now be NA to download files older than 24 hours
- hourly/solar timestamp now processed automatically
- use cases vignette added
- vignettes precompiled + CRAN tests removed to reduce automated load on DWD Server
- `indexFTP`: got much smarter in discerning files from folders
- indexes expanded for new DWD data
- New function: `readVars`, along with `parameter_abreviations`


# Version 0.10.0 (2018-03-26)

- created new index with sub-hourly data, reflected in data overview at `selectDWD`
- `dataDWD`,`readDWD`,`selectDWD`: file ending checks more elegant
- `createIndex`: updated to handle the new files
- some minor improvements, see <https://github.com/brry/rdwd/commits>


# Version 0.9.0 (2017-11-03)

- `readDWD`: readDWD.data and readDWD.meta are now separate (non-exported) functions
- `readDWD`: *fread* default is now FALSE
- `dataDWD`: *fread* and *overwrite* arguments added
- references dutch meteo package and useR!2017 rdwd presentation
- importFrom entries completed
- DESCRIPTION: BugReports entry added


# Version 0.8.0 (2017-06-09)

- reflects DWD FTP update June 1
- `indexFTP`: ftp blockage chance reduced, data loss avoided, progress bar added
- errors/warnings/messages now often include a traceback
- `metaInfo`: tells about non-public files
- `createIndex` + `readDWD`: read meta files in German locale to handle Umlaute
- Index documentation and creation unified
- Vignettes and readme improved
- various minor improvements and fixes, see <https://github.com/brry/rdwd/commits>

renamed: `indexDWD` -> `indexFTP`  
added: `lldist`, `maxlldist`, `nearbyStations`  
no longer exported in NAMESPACE: `rowDisplay`, `dirDWD`  
removed: `fileDWD`, `geoIndexAll`


# Version 0.7.0 (2017-02-03)

- interactive map now colored by availability of recent file
- stations with slightly varying coordinates (<900 m apart) aggregated in `geoIndex`
- documentation corrections, improved messages in `fileDWD` and `dirDWD`
- map vignette expanded, package vignette abbreviated slightly
- `metaInfo`: printout greatly improved
- duplicate index entries removed
- New function: `rowDisplay`
- New object: `geoIndexAll`
- Removed object: `mapDWD` (to reduce package size)


# Version 0.6.1 (2017-01-24)

Initial release of the package, development can be seen at <https://github.com/brry/rdwd/compare/master@{2016-10-19}...master@{2017-01-24}>
- `selectDWD`: uses index information to find files matching a path or station criteria.
- With the returned path/file names, `dataDWD` and `readDWD` download and read data.
- `dirDWD` and `fileDWD`: control that no file will be overwritten and path messages are useful.
- `indexDWD`: lists all the files on the FTP server with DWD data.
- `createIndex`: uses that list to create `fileIndex`, `metaIndex` and `geoIndex`.
- `mapDWD`: is an interactive leaflet map also useful to explore datasets.

Note: `dataDWD` and `readDWD` were started in June 2016 within my misc package berryFunctions, from which they will be deleted after rdwd is on CRAN
