## Steps:
##
## (1) First get the area harvested/sown data using the R API (GetData)
##
## (2) Then get the seed rate data after loading them in to the data
##     base. (GetTableData).
##
## NOTE (Michael): The codes need to be converted to CPC from FCL.
##
##
## (3) Multiply the seed rate with the area harvested/sown in the
##     following year to get the seed used in the current year.
##
## (4) Save the data back.

library(data.table)
library(faosws)
library(faoswsFlag)
library(faoswsUtil)
library(magrittr)
library(igraph)
library(lme4)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
areaSownElementCode = "5025"
areaHarvestedElementCode = "5312"
seedElementCode = "5525"
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    ## Define directories
    apiDirectory = "~/Documents/Github/sws_r_api/r_modules/seed_imputation/faoswsSeed"
    packageDirectory = "~/Documents/Github/sws_seed/faoswsSeed/R/"
    
    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        # token = "7823c00b-b82e-47bc-8708-1be103ac91e4" # Michael's token
        token = "d0e1f76f-61a6-4183-981c-d0fec7ac1845" # Josh's token
    )
    R_SWS_SHARE_PATH = paste0(apiDirectory, "/..")

    ## Copy over scripts from package directory
    file.copy(from = dir(packageDirectory, pattern = ".*\\.R$",
                         full.names = TRUE),
              to = apiDirectory, overwrite = TRUE)

    ## Source copied scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
}

## Get seed, area, and climate data from the SWS
seed =
    getOfficialSeedData() %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3)
area =
    getAllAreaData() %>%
    imputeAreaSown(data = .)
climate = getWorldBankClimateData()

## Merge together the three different datasets and construct a mixed effects
## model
seedModelData =
    mergeAllSeedData(seedData = seed, area, climate) %>%
    .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]
seedLmeModel = 
    lmer(log(Value_measuredElement_5525) ~ Value_wbIndicator_SWS.FAO.TEMP +
             timePointYears + 
         (log(Value_measuredElement_5025)|cpcLvl3/measuredItemCPC:geographicAreaM49),
         data = seedModelData)

selectedSeed =
    getSelectedSeedData(swsContext.datasets[[1]]) %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3) %>%
    mergeAllSeedData(seedData = ., area, climate) %>%
    .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]

selectedSeed[, predicted :=
                 exp(predict(seedLmeModel, selectedSeed, allow.new.levels = TRUE))]

with(selectedSeed, plot(Value_measuredElement_5525, predicted))

selectedSeed[Value_measuredElement_5525 >= 6e6 & predicted <= 4e6, ]

saveSeedData(selectedSeed)
