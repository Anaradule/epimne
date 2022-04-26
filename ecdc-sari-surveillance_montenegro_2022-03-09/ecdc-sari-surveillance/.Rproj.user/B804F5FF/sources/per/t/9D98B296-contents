#################################################################
############## MONTENEGRO - SARI EXCEL TO TESSY CSV #############
#################################################################
#
# Project:  Montenegro ECDC SARI surveillance
# Purpose:  Convert xls data to csv in TESSy format
# Country:  Montenegro - aggregated
# Created:  21 Feb 2022
# Updated:  23 Feb 2022
# Authors:  Mathias Leroy
# Rversion: 4.1.2
#
# Version control:
# GitHub: https://github.com/Epiconcept-Paris/ecdc-sari-surveillance
# Please ensure your working dir is correctly set and you have the required folder.
#
#################################################################
#################################################################

### PARAMETERS ========================================


### INSTALL & LOAD LIBRARIES ========================================
if(Sys.getenv("USERNAME")=='mat') {
  .libPaths(c('C:/Programs/rlibrary')) ## Folder for R libraries, comment this to use defaults
  options(repos = c(CRAN = "http://cran.rstudio.com/"))
  }
p <- lapply( c("readxl", "dplyr", "tidyr"
              , "openxlsx" ## required for getSheetNames()
  ), function(x) {suppressMessages(
    if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  })})
### CHANGE WD IF NEEDED -----
if(endsWith(getwd(), '/scripts')) setwd('../') 
if(endsWith(getwd(), '/ecdc-sari-surveillance')) setwd('./Montenegro/inflsariaggr/')
print(getwd())

### IMPORT DATA ========================================
fname <- "sources/SARI Ukupno MNE tabelaEpi.xlsx"

### LIST ALL SHEET NAMES -----
shtnames <- getSheetNames(fname)
# shtnames <- shtnames[1:3] ## limit for testing faster

### FOR EACH SHEET IN THE XLSX FILE -----
print('')
print('IMPORTING DATA...')
df_master <- data.frame() ## this dataframe is to store results
for(ii in 1:length(shtnames)){
  shtname <- shtnames[ii]

  ### READ 1 SHEET -----
  suppressMessages( ## this declutters the log
    df <- read_xlsx(path=fname, sheet=shtname, range="A45:R51", col_names=FALSE) ## it goes faster with sheet=ii than sheet=shtname
    )

  ### COMPUTE YEAR WEEK -----
  sheetnumber <- as.integer(gsub('N', '', shtname)) ## add the sheetname before appending

  thisyear <- as.integer(format(Sys.Date(), "%Y"))
  thisweek <- as.integer(format(Sys.Date(), "%W")) ## not used yet # todo: automatize currentweek or parametrize 39
  if(sheetnumber>39) thisyear <- thisyear-1 ## if week is above current week then it's previous year
  df$DateUsedForStatistics <- paste0(thisyear,'-W',sprintf("%02d", sheetnumber))

  ### COMPUTE SUM OF ALL COLUMNS & REMOVE EMPTY LINES -----
  df <- df %>%
        mutate(sum = rowSums(across(where(is.numeric))) ) %>%
        filter(sum != 0)

  print(paste0(ii, ') ', shtname, ' ', nrow(df), 'x', ncol(df), ' total:', sum(df$sum) ))

  ### APPEND ALL TOGETHER -----
  df_master <- rbind(df_master, df)
}
df <- df_master
print( paste(dim(df), collapse=" x "))



### TRANSFORM DATA ========================================
print('')
print('TRANSFORMING DATA...')

### RENAME & SORT -----
names(df) <- c("AgeGroup"
  , "DenomHospAdmissions", "NumSariHospitalisations", "NumSariICUadmissions"
  , "SARITestedSARSCoV2", "NumSpecimensSARSCoV2DetectSARI", "SarsCov2Neg"
  , "NumSpecimensTestedFlu", "InfluenzaA", "InfluenzaB", "InfluenzaNeg"
  , "OtherTotal", "NumSpecimensRSVDetect", "OtherOther"
  , "NumSariDeaths", "DeathsSarsCov2", "DeathsInfluenza", "DeathsOther"
  , "DateUsedForStatistics", "sum")
df <- df %>% arrange(DateUsedForStatistics, AgeGroup) ## sort

### SUM INFLUENZA A & B -----
df$NumSpecimensFluDetect = df$InfluenzaA + df$InfluenzaB

### RECODE AGE GROUPS -----
df <- df %>% mutate(AgeGroup=recode(AgeGroup , '0-4'='00-04', '5-14'='05-14'))

### LONG TO WIDE & AGGREGATE & RENAME -----
df <- df %>%
  # filter(AgeGroup != 'Ukupno') %>% ## removing Ukupno (means totals)
  select(c(
    DateUsedForStatistics, # This column will be kept
    AgeGroup,              # this column will be appended to the pivotted variables
    # and these are the pivotted variables:
    DenomHospAdmissions,
    NumSariHospitalisations, NumSariICUadmissions, NumSariDeaths,
    NumSpecimensFluDetect, NumSpecimensTestedFlu, NumSpecimensRSVDetect,
    NumSpecimensSARSCoV2DetectSARI, SARITestedSARSCoV2)) %>%

  ### PIVOT TO WIDE -----
  pivot_wider(names_from = AgeGroup
            , values_from = c(DenomHospAdmissions, NumSariHospitalisations, NumSariICUadmissions, NumSariDeaths,
                              NumSpecimensFluDetect, NumSpecimensTestedFlu, NumSpecimensRSVDetect,
                              NumSpecimensSARSCoV2DetectSARI, SARITestedSARSCoV2)
            , names_sep = "Age" ## Write 'Age' between variable and value to get the TESSy variable name faster
            , values_fn = sum
            ) %>%

  ### TOTAL COLUMNS (these include unkowns) -----
  rename(
    DenomHospAdmissions = paste0('DenomHospAdmissions','Age','Ukupno'),
    NumSariHospitalizations = paste0('NumSariHospitalisations','Age','Ukupno'),
    NumSariICUadmissions = paste0('NumSariICUadmissions','Age','Ukupno'),
    NumSariHospitalisationsDeaths = paste0('NumSariDeaths','Age','Ukupno'),
    NumSpecimensTotFluDetect = paste0('NumSpecimensFluDetect','Age','Ukupno'),
    NumSpecimensTotSARI = paste0('NumSpecimensTestedFlu','Age','Ukupno'),
    NumSpecimensRSVDetectSARI = paste0('NumSpecimensRSVDetect','Age','Ukupno'),
    NumSpecimensSARSCoV2DetectSARITotal = paste0('NumSpecimensSARSCoV2DetectSARI','Age','Ukupno'),
    SARITestedSARSCoV2Total = paste0('SARITestedSARSCoV2','Age','Ukupno'),
    ) %>%
  # mutate( NumSariHospitalizations = rowSums(.[grep("NumSariHospitali", names(.))], na.rm = TRUE),
  #         NumSariICUadmissions = rowSums(.[grep("NumSariICUadmissionsAge", names(.))], na.rm = TRUE),
  #         NumSariHospitalisationsDeaths = rowSums(.[grep("NumSariDeathsAge", names(.))], na.rm = TRUE),
  #         NumSpecimensTotFluDetect = rowSums(.[grep("NumSpecimensFluDetectAge", names(.))], na.rm = TRUE),
  #         NumSpecimensTotSARI = rowSums(.[grep("NumSpecimensTestedFluAge", names(.))], na.rm = TRUE),
  #         NumSpecimensRSVDetectSARI = rowSums(.[grep("NumSpecimensRSVDetectAge", names(.))], na.rm = TRUE),
  #         NumSpecimensSARSCoV2DetectSARITotal = rowSums(.[grep("NumSpecimensSARSCoV2DetectSARIAge", names(.))], na.rm = TRUE),
  #         SARITestedSARSCoV2Total = rowSums(.[grep("SARITestedSARSCoV2Age", names(.))], na.rm = TRUE),
  #         ) %>% # not correct, not including unkowns

  ### ADD STL AFTER SOME VAR NAMES -----
  rename_at(vars(starts_with("NumSariHospitali")), function(x) { paste0(x,'STL') } ) %>%
  rename_at(vars(starts_with("NumSariDeathsAge")), function(x) { paste0(x,'STL') } ) %>%
  rename_at(vars(starts_with("DenomHospAdmissions")), function(x) { paste0(x,'STL') } ) %>%
  rename()


### ADD EMPTY VARIABLES -----

df$'DenomHospPopulationAge00-04STL'    <- ''
df$'DenomHospPopulationAge05-14STL'    <- ''
df$'DenomHospPopulationAge15-29STL'    <- ''
# df$'DenomHospPopulationAge15-64STL'    <- ''
df$'DenomHospPopulationAge30-64STL'    <- ''
# df$'DenomHospPopulationAge65+STL'      <- ''
df$'DenomHospPopulationAge65-79STL'    <- ''
df$'DenomHospPopulationAge80+STL'      <- ''
df$'DenomHospPopulationSTL'            <- ''

df$'NumSpecimensMERSDetectSARI'  <- ''
df$'NumSpecimensTestedMERS'      <- ''

df$'NumSpecimensTestedRSVAge00-04'   <- ''
df$'NumSpecimensTestedRSVAge05-14'   <- ''
df$'NumSpecimensTestedRSVAge15-29'   <- ''
# df$'NumSpecimensTestedRSVAge15-64'   <- ''
df$'NumSpecimensTestedRSVAge30-64'   <- ''
# df$'NumSpecimensTestedRSVAge65+'     <- ''
df$'NumSpecimensTestedRSVAge65-79'   <- ''
df$'NumSpecimensTestedRSVAge80+'     <- ''
df$'NumSpecimensTotRSV'              <- ''

df$'NumSariRepSites'      <- ''
df$'ReportingFraction'    <- ''
df$'DescriptionSARI'      <- ''

df$'NumSpecimensAH1DetectSARI'       <- ''
df$'NumSpecimensAH1N1DetectSARI'     <- ''
df$'NumSpecimensAH3DetectSARI'       <- ''
df$'NumSpecimensAH3N2DetectSARI'     <- ''
df$'NumSpecimensAUnkDetectSARI'      <- ''
df$'NumSpecimensBDetectSARI'         <- ''
df$'NumSpecimensBVICDetectSARI'      <- ''
df$'NumSpecimensBYAMDetectSARI'      <- ''
df$'NumSpecimensSWOAH1DetectSARI'    <- ''
df$'NumSpecimensSWOAH1N1DetectSARI'  <- ''


### ADD METADATA VARIABLES -----
df$RecordType        <- 'INFLSARIAGGR'
df$Subject           <- 'INFLSARI'
df$DataSource        <- 'ME-MNE(SARI)'
df$RecordTypeVersion <- '3'
df$ReportingCountry  <- 'ME'


### REORDER VARIABLES -----
print( paste(dim(df), collapse=" x "))

df <- df %>%
  select(DateUsedForStatistics,
         RecordType,
         RecordTypeVersion,
         Subject,
         DataSource,
         ReportingCountry,
         DescriptionSARI,
         ReportingFraction,
         NumSariRepSites,
         starts_with("NumSariHospitalisationsAge"), NumSariHospitalizationsSTL,
         starts_with("NumSariICUadmissionsAge"), NumSariICUadmissions,
         starts_with("NumSariDeathsAge"), NumSariHospitalisationsDeathsSTL,
         starts_with("DenomHospAdmissionsAge"), DenomHospAdmissionsSTL,
         starts_with("DenomHospPopulationAge"), DenomHospPopulationSTL,
         starts_with("NumSpecimensSARSCoV2DetectSARIAge"), NumSpecimensSARSCoV2DetectSARITotal,
         starts_with("SARITestedSARSCoV2Age"), SARITestedSARSCoV2Total,
         starts_with("NumSpecimensFluDetectAge"), NumSpecimensTotFluDetect,
         NumSpecimensAUnkDetectSARI,
         NumSpecimensSWOAH1N1DetectSARI,
         NumSpecimensAH3N2DetectSARI,
         NumSpecimensBDetectSARI,
         NumSpecimensBVICDetectSARI,
         NumSpecimensBYAMDetectSARI,
         NumSpecimensAH1N1DetectSARI,
         NumSpecimensSWOAH1DetectSARI,
         NumSpecimensAH1DetectSARI,
         NumSpecimensAH3DetectSARI,
         starts_with("NumSpecimensTestedFluAge"), NumSpecimensTotSARI,
         starts_with("NumSpecimensRSVDetectAge"), NumSpecimensRSVDetectSARI,
         starts_with("NumSpecimensTestedRSVAge"), NumSpecimensTotRSV,
         NumSpecimensMERSDetectSARI,
         NumSpecimensTestedMERS)

# glimpse(df)
# df
print( paste(dim(df), collapse=" x "))
# print(dim(df))


### EXPORT DATA ========================================
print('')
print('EXPORTING DATA...')
write.table(df, "data/ME-INFLSARIAGGR.csv", na = "", row.names = FALSE, col.names = TRUE, append = FALSE, sep = ",")


print('')
print('=== DONE ===')



### QUESTIONS -----

# DataSource = 'ME-SARISURV' or ME-MNE(SARI) ?

# each year the limit = week 39 ?






### LIST OF VARIABLES ---


### REQUIRED TRUE ---
# v RecordType
# v Subject
# v DataSource
# v ReportingCountry
# v DateUsedForStatistics

### REQUIRED WARNING ---
# ? DenomHospAdmissionsSTL
# ? DenomHospPopulationSTL
# v NumSariHospitalisationsDeathsSTL : Total number of hospital SARI deaths (all ages)
# v NumSariHospitalizationsSTL
# v NumSariICUadmissions
# ? NumSariRepSites : Total number of reporting sites



### FOUND ================

# RecordType              --> INFLSARIAGGR
# RecordTypeVersion       --> 3
# Subject                 --> INFLSARI
# DataSource              --> ME-SARISURV ?
# ReportingCountry        --> ME
# DateUsedForStatistics   --> yyyy-W + sheet number

# NumSariHospitalisationsAge00-04STL    --> Col B
# NumSariHospitalisationsAge05-14STL    -->
# NumSariHospitalisationsAge15-29STL    -->
# NumSariHospitalisationsAge15-64STL    -->
# NumSariHospitalisationsAge30-64STL    -->
# NumSariHospitalisationsAge65+STL      -->
# NumSariHospitalisationsAge65-79STL    -->
# NumSariHospitalisationsAge80+STL      -->
# NumSariHospitalizationsSTL            --> sum all ages  (! z not s here)

# NumSariICUadmissionsAge00-04    --> Col D
# NumSariICUadmissionsAge05-14    -->
# NumSariICUadmissionsAge15-29    -->
# NumSariICUadmissionsAge15-64    -->
# NumSariICUadmissionsAge30-64    -->
# NumSariICUadmissionsAge65+      -->
# NumSariICUadmissionsAge65-79    -->
# NumSariICUadmissionsAge80+      -->
# NumSariICUadmissions            --> sum all ages

# NumSariDeathsAge00-04STL   --> Col O ? (not sure)
# NumSariDeathsAge05-14STL   -->
# NumSariDeathsAge15-29STL   -->
# NumSariDeathsAge15-64STL   -->
# NumSariDeathsAge30-64STL   -->
# NumSariDeathsAge65+STL     -->
# NumSariDeathsAge65-79STL   -->
# NumSariDeathsAge80+STL     -->
# NumSariHospitalisationsDeathsSTL    --> sum all age (not sure)

# NumSpecimensFluDetectAge00-04   --> Col I + J
# NumSpecimensFluDetectAge05-14   -->
# NumSpecimensFluDetectAge15-29   -->
# NumSpecimensFluDetectAge15-64   -->
# NumSpecimensFluDetectAge30-64   -->
# NumSpecimensFluDetectAge65+     -->
# NumSpecimensFluDetectAge65-79   -->
# NumSpecimensFluDetectAge80+     -->
# NumSpecimensTotFluDetect        --> sum all ages

# NumSpecimensTestedFluAge00-04    --> col H
# NumSpecimensTestedFluAge05-14    -->
# NumSpecimensTestedFluAge15-29    -->
# NumSpecimensTestedFluAge15-64    -->
# NumSpecimensTestedFluAge30-64    -->
# NumSpecimensTestedFluAge65+      -->
# NumSpecimensTestedFluAge65-79    -->
# NumSpecimensTestedFluAge80+      -->
# NumSpecimensTotSARI        --> sum all ages

# NumSpecimensRSVDetectAge00-04   --> Col M
# NumSpecimensRSVDetectAge05-14   -->
# NumSpecimensRSVDetectAge15-29   -->
# NumSpecimensRSVDetectAge15-64   -->
# NumSpecimensRSVDetectAge30-64   -->
# NumSpecimensRSVDetectAge65+     -->
# NumSpecimensRSVDetectAge65-79   -->
# NumSpecimensRSVDetectAge80+     -->
# NumSpecimensRSVDetectSARI       --> sum all ages

# NumSpecimensSARSCoV2DetectSARIAge00-04   --> Col F
# NumSpecimensSARSCoV2DetectSARIAge05-14   -->
# NumSpecimensSARSCoV2DetectSARIAge15-29   -->
# NumSpecimensSARSCoV2DetectSARIAge15-64   -->
# NumSpecimensSARSCoV2DetectSARIAge30-64   -->
# NumSpecimensSARSCoV2DetectSARIAge65+     -->
# NumSpecimensSARSCoV2DetectSARIAge65-79   -->
# NumSpecimensSARSCoV2DetectSARIAge80+     -->
# NumSpecimensSARSCoV2DetectSARITotal      --> sum all ages

# SARITestedSARSCoV2Age00-04    --> Col E
# SARITestedSARSCoV2Age05-14    -->
# SARITestedSARSCoV2Age15-29    -->
# SARITestedSARSCoV2Age15-64    -->
# SARITestedSARSCoV2Age30-64    -->
# SARITestedSARSCoV2Age65+      -->
# SARITestedSARSCoV2Age65-79    -->
# SARITestedSARSCoV2Age80+      -->
# SARITestedSARSCoV2Total       --> sum all ages

# DescriptionSARI                   --> skip




### NOT FOUND ================

# DenomHospAdmissionsAge00-04STL    --> ??? doesn't have that info
# DenomHospAdmissionsAge05-14STL    --> ???
# DenomHospAdmissionsAge15-29STL    --> ???
# DenomHospAdmissionsAge15-64STL    --> ???
# DenomHospAdmissionsAge30-64STL    --> ???
# DenomHospAdmissionsAge65+STL      --> ???
# DenomHospAdmissionsAge65-79STL    --> ???
# DenomHospAdmissionsAge80+STL      --> ???
# DenomHospAdmissionsSTL            --> ???

# DenomHospPopulationAge00-04STL    --> ??? doesn't have that info
# DenomHospPopulationAge05-14STL    --> ???
# DenomHospPopulationAge15-29STL    --> ???
# DenomHospPopulationAge15-64STL    --> ???
# DenomHospPopulationAge30-64STL    --> ???
# DenomHospPopulationAge65+STL      --> ???
# DenomHospPopulationAge65-79STL    --> ???
# DenomHospPopulationAge80+STL      --> ???
# DenomHospPopulationSTL            --> ???

# NumSpecimensMERSDetectSARI  --> ???
# NumSpecimensTestedMERS      --> ???

# NumSpecimensTestedRSVAge00-04     --> ??? doesn't have that info
# NumSpecimensTestedRSVAge05-14     -->
# NumSpecimensTestedRSVAge15-29     -->
# NumSpecimensTestedRSVAge15-64     -->
# NumSpecimensTestedRSVAge30-64     -->
# NumSpecimensTestedRSVAge65+       -->
# NumSpecimensTestedRSVAge65-79     -->
# NumSpecimensTestedRSVAge80+       -->
# NumSpecimensTotRSV                --> ???

# NumSariRepSites                     --> skip
# ReportingFraction                   --> ???

# NumSpecimensAH1DetectSARI       --> ???
# NumSpecimensAH1N1DetectSARI     --> ???
# NumSpecimensAH3DetectSARI       --> ???
# NumSpecimensAH3N2DetectSARI     --> ???
# NumSpecimensAUnkDetectSARI      --> ???
# NumSpecimensBDetectSARI         --> ???
# NumSpecimensBVICDetectSARI      --> ???
# NumSpecimensBYAMDetectSARI      --> ???
# NumSpecimensSWOAH1DetectSARI    --> ???
# NumSpecimensSWOAH1N1DetectSARI  --> ???


