

### INSTALL & LOAD LIBRARIES ========================================
if(Sys.getenv("USERNAME")=='mat') {
  .libPaths(c('C:/Programs/rlibrary')) ## Folder for R libraries, comment this to use defaults
  options(repos = c(CRAN = "http://cran.rstudio.com/"))
  }
p <- lapply( c("readxl", "readr", "dplyr"#, "tidyr"
  ), function(x) {suppressMessages(
    if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  })})
### CHANGE WD IF NEEDED -----
if(endsWith(getwd(), '/scripts')) setwd('../') 
if(endsWith(getwd(), '/ecdc-sari-surveillance')) setwd('./Montenegro/inflsariaggr/')
print(getwd())


selectweek = '2021-W50'

df1 <- read_csv("data/ME-INFLSARIAGGR.csv")
df2 <- read_xlsx(path='sources/20220224_vINFLSARIAGGR_ME.xlsx')

df3 <- bind_rows(df1, df2) %>% ## APPEND
	filter(DateUsedForStatistics==selectweek) %>% ## SELECT ONLY 1 WEEK
	# select(c( 													## TO HAVE FEWER COLUMNS
	# 	DenomHospAdmissionsSTL,
	# 	NumSariHospitalizationsSTL, NumSariICUadmissions,
	# 	NumSariHospitalisationsDeathsSTL, NumSpecimensTotFluDetect,
	# 	NumSpecimensTotSARI, NumSpecimensRSVDetectSARI,
	# 	NumSpecimensSARSCoV2DetectSARITotal, SARITestedSARSCoV2Total,
	# 	# 'NumSariHospitalisationsAge30-64STL',
	# )) %>%
	select(order(colnames(.))) %>% ## order columns alphabetically
	rename()

glimpse(df3)
# print(df3)

# print(df1 %>% select(matches('ariHospitali')))
