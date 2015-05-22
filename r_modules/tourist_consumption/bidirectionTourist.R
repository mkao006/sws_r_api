## Code to calculate bidirection tourist calorie consumption, revised 
## version that handles multiple years data from all countries

## Created by JAM 19/2/2015 at FAO Rome
## Last updated on 5/3/2015 by JAM

## Load required functions
library(faosws)
library(dplyr)
library(reshape2)
library(data.table)
library(faoswsUtil)

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    token = "01ed4530-8f0c-4334-b426-1b2cc30e259b"
    GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws",token)
}

## set the keys to get the tourist data from the FAO working system
destinationAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
    "destinationCountryM49")
originAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
    "originCountryM49")
tourismElementCodes <- faosws::GetCodeList("tourism", "tourist_consumption",
    "tourismElement")

## set the year range to pull data from the SWS
yearRange <- swsContext.datasets[[1]]@dimensions$timePointYears@keys

##Pull the bidirectional movement data from SWS pertaining to tourist visitors
##to all countries
dim1 <- Dimension(name = "destinationCountryM49",
                  keys = destinationAreaCodes[, code])
dim2 <- Dimension(name = "originCountryM49", keys = originAreaCodes[, code])
dim3 <- Dimension(name = "tourismElement", keys = ("60"))
dim4 <- Dimension(name = "timePointYears", keys = yearRange)
key <- DatasetKey(domain = "tourism", dataset = "tourist_flow",
                  dimensions = list(dim1, dim2, dim3, dim4))

## download the first tourist data from the SWS
data1 <- GetData(key, flags = FALSE)

## remove the tourismElement column which is of no value to me here
data1 <- data1[, which(!grepl("tourism", colnames(data1))), with=FALSE]

## change column names to small simple ones representing destination, origin,
## year and overnight visitor number
setnames(data1, old = colnames(data1), new = c("dest", "orig", "year", "onVisNum"))

## set the keys to get the tourist data from the FAO working system
areaCodes = faosws::GetCodeList("tourism", "tourist_consumption",
    "geographicAreaM49")

## Pull number of mean number of days stayed, and number of single-day visitors
## from SWS pertaining to visitors to all countries
dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes [, code])
dim2 = Dimension(name = "tourismElement", keys = c("20", "30"))
dim3 = Dimension(name = "timePointYears", keys = yearRange)
key = DatasetKey(domain = "tourism", dataset = "tourist_consumption",
                  dimensions = list(dim1, dim2, dim3))

## download the bi-direction tourism data from the SWS
data2 <- GetData(key, flags = FALSE)

## set the column names to small simple ones representing destination, database
## element, year and value
setnames(data2, old = colnames(data2), new = c("dest", "element", "year", "value"))

## cast the data table to get it in long format
data3 <- as.data.table(dcast(data2, dest + year ~ element))

## change the column names to something readable, "onVisDays" stands for the
## mean number of days that overnight visitors stayed in the destination country.
## "totDayVisNum" is the number of people who visited the country but for a
## single day, e.g. from a cruise ship
setnames(data3, old = colnames(data3), new = c("dest", "year", "onVisDays",
                                           "totDayVisNum"))

## replace missing day visitor numbers (NA) with zero, because it won't effect
## end calculations, but NA's cause equations to fail
data3$totDayVisNum[is.na(data3$totDayVisNum)] <- 0

## merge the two data sets, one containing overnight visitor numbers and number
## of days they visited, the other data set the number of tourists travelling to
## and from each country
data4 <- merge(data1, data3, by=c("dest", "year"), all.x = TRUE)

## rearrange the column order to make it easier to view
data4 <- setcolorder(data4, neworder = c("year", "orig", "dest", "onVisNum",
                                "onVisDays", "totDayVisNum"))

## a small number of countries are missing values for overnight visitor days,
## "onVisDays" and this affects the bi-directional calculations for them, but
## also all of the other countries as well, so this imputes the missing number
## of days, by taking the mean of all day numbers present, grouped by year.
data4[, onVisDays := ifelse(is.na(onVisDays), mean(onVisDays, na.rm=TRUE),
                            onVisDays), by = year]

## calculate the total number tourist visitor days, the product of overnight
## visitor number and days per visit
data4[, onVisTotDays := onVisNum * onVisDays]

## calculate a new total overnight visitor number per country of destination, to
## be used later to proportion the day visitor number, because we do not have
## data for country of origin, and allocate them to a country of origin,
## assuming they arrive in the same relative proportions as the overnight
## visitors
data4[, totOnVisNum := sum(onVisNum), by=list(year,dest)]

## create a new total visitor days by summing the overnight viistor days, and
## the day visitor days
data4[, totVisDays := onVisTotDays + totDayVisNum]

## set the keys to get the calorie consuption, by individual FBS commodity for
## each country from the FAO working system
foodAreaCodes <- faosws::GetCodeList("suafbs", "fbs", "geographicAreaM49")
foodElementCodes <- faosws::GetCodeList("suafbs", "fbs", "measuredElementSuaFbs")
## the Item codes contain a hierarchy.  We need to determine all the child
## nodes of the hierarchy and add them to get total consumption.
foodItemTree <- GetCodeTree("faostat_one", "FS1_SUA", "measuredItemFS")
oldAreaCodes <- GetCodeList("faostat_one", "FS1_SUA", "geographicAreaFS")
foodItemTree <- adjacent2edge(foodItemTree)
children <- setdiff(foodItemTree$children, foodItemTree$parent)

##Pull the supply utilization account(SUA) food balance sheet (FBS) data from
##SWS pertaining to calorie consumption from each commodity in each country
dim1 <- Dimension(name = "geographicAreaFS",
                  keys = oldAreaCodes[type == "country", code])
## A bit hackish: get population from total calories and total calories/person/day
dim2 <- Dimension(name = "measuredElementFS", keys = c("261", "264"))
dim3 <- Dimension(name = "measuredItemFS", keys = children)
dim4 <- Dimension(name = "timePointYears", keys = yearRange)
key <- DatasetKey(domain = "faostat_one", dataset = "FS1_SUA",
                  dimensions = list(dim1, dim2, dim3, dim4))

## download the calorie consumption data from the SWS
data6 <- GetData(key, flags = FALSE)

data6 <- dcast.data.table(data6,
            geographicAreaFS + measuredItemFS + timePointYears ~ measuredElementFS,
            value.var = "Value")
setnames(data6, c("261", "264"), c("totalCal", "calPerPersonPerDay"))
data6[, population := totalCal / 365 / calPerPersonPerDay * 1e6]
data6[, population := mean(population, na.rm = TRUE), by = "geographicAreaFS"]
data6[, totalCal := NULL]

## Convert the area codes and item codes to M49 and CPC
areaMap = GetTableData(schemaName = "ess", tableName = "fal_2_m49")
itemMap = GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")
data6[, measuredItemFS := formatC(as.numeric(measuredItemFS), width = 4,
                                  format = "g", flag = "0")]
setkeyv(data6, "measuredItemFS")
setnames(itemMap, "fcl", "measuredItemFS")
setkeyv(itemMap, "measuredItemFS")
data6 = merge(data6, itemMap)
setnames(data6, "cpc", "measuredItemCPC")
data6[, measuredItemFS := NULL]

## And now the area codes
setkeyv(data6, "geographicAreaFS")
setnames(areaMap, "fal", "geographicAreaFS")
setkeyv(areaMap, "geographicAreaFS")
data6 = merge(data6, areaMap)
setnames(data6, "m49", "geographicAreaM49")
data6[, geographicAreaFS := NULL]

## set the column names to small simple ones representing destination, database
## element, year and value
setnames(data6, old = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears", "calPerPersonPerDay"),
         new = c("orig", "item", "year", "calValue"))

## Compute total calories per person per day in orig country
data6[, totalLocalCalories := sum(calValue), by = c("orig", "year")]

## set the keys for the data.table to sort by year and country of origin
setkey(data6, year, orig, item)

## merge data4 and data6 to allow calculation of calories by commodity
data7 <- merge(data4, data6, allow.cartesian=TRUE, by = c("year", "orig"))
## Get rid of some of the tourist columns that we don't need anymore:
data7[, c("onVisNum", "onVisDays", "totDayVisNum",
          "onVisTotDays", "totOnVisNum") := NULL]

## calculate the total calories consumed, by item, for the entire year
data7[, totCaloriesByItemPerYear := totVisDays * calValue]

## calculate calories consumed within a country, by tourists visiting from other
## countries, and therefore these calories can be subtracted from the total
## calories available to the permanent population in the country listed in
## column #3
touristTotalCaloriesInCountry <- data4[, list(calsVisToCountry = sum(totVisCals)),
                                  by = list(year,dest)]

## calculate calories by people that left the country to be a tourist in the
## destination country, therefore these calories can be added to the total
## calories available to the permanent population country of origin, which is
## listed in column
touristTotalCaloriesOutCountry <- data4[, list(calsVisFromCountry = sum(totVisCals)),
                                  by = list(year,orig)]

## sort the calories consumed within a country, by individual food balance sheet
## commodity item, by tourists visiting from other countries, and therefore these
## calories can be subtracted from the total calories available to the permanent
## population in the country listed in column #2, dest
touristCaloriesInCountryByItem <- data7[ , list(totCaloriesByItemPerYear),
                                        by = list(year, dest, orig, item) ]

## sort the calories consumed by visitors from country of origin, by individual
## food balance sheet commodity item, by tourists visiting other countries, and
## therefore these calories can be subtracted from the total calories available
## to the permanent population in the country listed in column #2, orig
touristCaloriesOutCountryByItem <- data7[ , list(totCaloriesByItemPerYear),
                                        by = list(year, orig, dest, item) ]

## this is a crosscheck of calorie totals summed by dest, orig and year in data7
## to make sure they corroborate with the originals in data4
crosscheck <- data7[, sum(totCaloriesByItemPerYear),
                     by = list(dest, orig, year)]

## assemble two outputs into a list to be returned
touristCalories <- list(touristTotalCaloriesInCountry,
                        touristTotalCaloriesOutCountry)

## assemble two outputs into a list to be returned
touristCaloriesByItem <- list(touristCaloriesInCountryByItem,
                              touristCaloriesOutCountryByItem)

"Module completed!"