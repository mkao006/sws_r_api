library(data.table)
library(faosws)
library(dplyr)
library(faoswsUtil)
#library(reshape2)

## To do:
## - Check the input dataset from Josef against what's on the server: ask Nick for
## the file that Jim gave him with the elasticities, compare with Josef's file
## and figure out why we have differences.  Reload if necessary.
## - Come up with a way to get commodity trees out of the system (additional
## table? different hierarchy?)

## set up for the test environment and parameters
R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE <- Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == "") {
    token <- "c2307a1a-e69e-4231-afbd-e19c9b9021aa"
    GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws",token)
    TEST_MODE <- TRUE
    if(Sys.info()[7] == "josh") # Josh's work computer
        files = dir("~/Documents/Github/sws_r_api/r_modules/food_imputation/R",
                    full.names = TRUE)
    if(Sys.info()[7] == "rockc_000") # Josh's laptop
        files = dir("~/Github/sws_r_api/r_modules/food_imputation/R",
                    full.names = TRUE)
    sapply(files, source)
}

## Define the keys that we'll need for all the dimensions
## set the keys to get the population data from the FAO working system
areaCodesM49 <- GetCodeList("population", "population", "geographicAreaM49")
## Filter areaCodes to "country" type only:
areaCodesM49 <- areaCodesM49[type == "country", code]
## We need different area codes for the SUA domain
areaCodesFS <- GetCodeList("faostat_one", dataset = "FS1_SUA_UPD",
                           dimension = "geographicAreaFS")
areaCodesFS <- areaCodesFS[type == "country", code]
yearsForSD <- as.numeric(swsContext.computationParams$yearsForVar)
## We will need the year of imputation as well as previous years to compute the
## standard deviation.  We'll grab as many years as specified by the user.
yearCodes <- as.numeric(swsContext.computationParams$yearToProcess) +
    (-yearsForSD:0)
yearCodes <- as.character(yearCodes)
## GDP per capita (constant 2500 US$) is under this key
gdpCodes <- "NY.GDP.PCAP.KD"
## The element 21 contains the FBS population numbers
populationCodes <- "21"
## The element 141 contains the FBS food numbers
foodCodes <- "141"
suaCodes <- GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")$code
comCodes <- GetCodeList("food", "food_factors","foodCommodityM")$code
fdmCodes <- GetCodeList("food", "food_factors","foodFdm")$code
funCodes <- GetCodeList("food", "food_factors","foodFunction")$code
varCodes <- "y_e" ## Only need elasticities from the food domain table

## Define the dimensions
dimM49 <- Dimension(name = "geographicAreaM49", keys = areaCodesM49)
dimFS <- Dimension(name = "geographicAreaFS", keys = areaCodesFS)
dimPop <- Dimension(name = "measuredElementPopulation", keys = populationCodes)
dimTime <- Dimension(name = "timePointYears", keys = yearCodes)
dimGDP <- Dimension(name = "wbIndicator", keys = gdpCodes)
dimFood <- Dimension(name = "measuredElementFS", keys = foodCodes)
dimSua <- Dimension(name = "measuredItemFS", keys = suaCodes)
dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)

## Define the pivots.  We won't need this for all dimensions, so we'll only
## define the relevant ones.
pivotM49 <- Pivoting(code = "geographicAreaM49")
pivotPop <- Pivoting(code = "measuredElementPopulation")
pivotTime <- Pivoting(code = "timePointYears")
pivotGDP <- Pivoting(code = "wbIndicator")

## Define the keys
keyPop <- DatasetKey(domain = "population", dataset = "population",
                     dimensions = list(dimM49, dimPop, dimTime))
keyGDP <- DatasetKey(domain = "WorldBank", dataset = "wb_ecogrw",
                     dimensions = list(dimM49, dimGDP, dimTime))
keyFood <- DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                      dimensions = list(dimFS, dimFood, dimSua, dimTime))
keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))

## Download all the datasets:

## download the population data from the SWS.  Using the pivoting argument, we 
## can specify the column order.  Since we're only pulling one key for the 
## population dimension, it makes sense to use that as the last dimension with 
## normalized = FALSE.  Doing this makes the last column the population, and
## names it Value_measuredElementPopulation_21.  We'll just rename it to
## population.
popData <- GetData(keyPop, flags=FALSE, normalized = FALSE,
                   pivoting = c(pivotM49, pivotTime, pivotPop))
setnames(popData, "Value_measuredElementPopulation_21", "population")
## download the gdp data from the SWS.  We're again only pulling one wbIndicator
## dimension, so we'll do the same thing we did for population.
gdpData <- GetData(keyGDP, flags=FALSE, normalized = FALSE,
                   pivoting = c(pivotM49, pivotTime, pivotGDP))
setnames(gdpData, "Value_wbIndicator_NY.GDP.PCAP.KD", "GDP")
## download the food data from the SWS
foodData <- GetData(keyFood, flags = FALSE, normalized = TRUE)
setnames(foodData, "Value", "food")
## download the food dimension data (elasticities) from the SWS
fdmData <- GetData(keyFdm, flags=FALSE, normalized = FALSE)
setnames(fdmData, "Value_foodVariable_y_e", "elasticity")

## Merge the datasets together, and perform some processing.

## merge the current population and gross domestic product data into a single
## dataset
GdpPopData <- merge(popData, gdpData, all = TRUE,
                    by = c("geographicAreaM49", "timePointYears"))

# foodData$com_cod <-  rep(NA,length(foodData$com_sua_cod))
# foodData$fdm_cod <- rep(NA,length(foodData$com_sua_cod))

funcCodes <- lapply(foodData$measuredItemFS, sua_2_fbs_fdm)
foodData <- cbind(foodData, do.call("rbind", funcCodes))
foodData[, foodFdm := as.character(foodFdm)]
foodData[, foodCommodityM := as.character(foodCommodityM)]
foodData <- foodData[!is.na(foodFdm), ]
foodData[, geographicAreaM49 := fs2m49(as.character(geographicAreaFS))]
## Mapping creates some NA M49 codes.  Remove those rows, as they don't exist in
## the FBS domain.
foodData <- foodData[!is.na(geographicAreaM49), ]
foodData[, geographicAreaFS := NULL]

GdpPopFoodDataM49 = merge(foodData, GdpPopData, all.x = TRUE,
                          by = c("geographicAreaM49", "timePointYears"))

data_base <- merge(GdpPopFoodDataM49, fdmData,
                   by = c("foodCommodityM","foodFdm", "geographicAreaM49"), 
                   all.x = TRUE)

## HACK: Remove rows with missing func_form.  This may not be the right thing to
## do: we may need to update the elasticities table.
data_base <- data_base[!is.na(foodFunction), ]

## First, sort the data by area and time.  The time sorting is important as we
## will later assume row i+1 is one time step past row i.
setkeyv(data_base, c("geographicAreaM49", "timePointYears"))

## The funcional form 4 (originally presented in Josef's data) was replaced by
## functional form 3 The functional form 32 is a typo. It was replaced by
## functional form 2 in Food Factors database.
data_base$foodFunction<-ifelse(data_base$foodFunction==4,3,data_base$foodFunction)
data_base[, foodHat := calculateFood(food = .SD$food, elas = .SD$elasticity,
                                     gdp_pc = .SD$GDP/.SD$population,
                                     ## We can use the first value since they're all the same:
                                     functionalForm = .SD$foodFunction[1]),
          by = c("measuredItemFS", "geographicAreaM49")]  

# ## calculate calories  
# data$hat_cal_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_cal_pc_2012*
#                                       data$hat_food_pc_2013/data$hat_food_pc_2012,0)
# 
# ## calculate proteins 
# data$hat_prot_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_prot_pc_2012*
#                                        data$hat_food_pc_2013/data$hat_food_pc_2012,0)
#   
# ## calculate fats 
# data$hat_fat_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_fat_pc_2012*
#                                       data$hat_food_pc_2013/data$hat_food_pc_2012,0)                  


# In statistics, a forecast error is the difference between the actual or real
# and the predicted or forecast value of a time series or any other phenomenon
# of interest.
# In simple cases, a forecast is compared with an outcome at a single
# time-point and a summary of forecast errors is constructed over a collection
# of such time-points. Here the forecast may be assessed using the difference
# or using a proportional error.
# By convention, the error is defined using the value of the outcome minus the
# value of the forecast.
data_base[, error := food - foodHat]

# Geographic Area, measuredElement = 141, measuredItem = SUA item code, Dist
# Param = log(Mu) or log(Sigma), Year = Year
dataToSave <- data_base[,
    ## Since we're ordered by time, foodHat[.N] will give the last estimate for
    ## the food element, and this is exactly what we want.
    list(mean = foodHat[.N],
         var = mean(error^2, na.rm = TRUE),
         timePointYears = max(timePointYears)),
    by = c("geographicAreaM49", "measuredElementFS", "measuredItemFS")]

## To convert from mu/sigma to logmu/logsigma, we can use the method of moments
## estimators (which express the parameters of the log-normal distribution in
## terms of the moments).  The formula are:
## logmu = 2*log(E(X)) - 1/2*log(E(X^2))
## logsigma = log(E(X^2)) - 2*log(E(X))
## We can write E(X^2) = sigma^2 + E(X)^2, and so we can now easily express
## logmu and logsigma from the estimated expected value and variance.

dataToSave[, lmu := 2*log(mean)-1/2*log(var+mean^2)]
dataToSave[, lsi := log(var+mean^2) - 2*log(mean)]
## Remove mean and var now that we've calculate lmu and lsi
dataToSave[, c("mean", "var") := NULL]
dataToSave = data.table:::melt.data.table(dataToSave,
    measure.vars = c("lmu", "lsi"))

## Prepare data and save it to SWS
setnames(dataToSave, c("measuredElementFS", "variable", "value"),
         c("measuredElement", "fbsDistribParam", "Value"))
dataToSave[, fbsDistribParam := as.character(fbsDistribParam)]
dataToSave[, measuredItemFS := formatC(as.numeric(measuredItemFS), width = 4,
                                       format = "g", flag = "0")]
dataToSave[, measuredItemCPC := fcl2cpc(measuredItemFS)]
dataToSave[, measuredItemFS := NULL]
setcolorder(dataToSave, c("geographicAreaM49", "measuredElement",
                          "measuredItemCPC", "fbsDistribParam",
                          "timePointYears", "Value"))
SaveData(domain = "suafbs", dataset = "fbs_distrib", data = dataToSave)
