## Code to calculate bidirection tourist calorie consumption, revised 
## version that handles multiple years data from all countries

## Created by JAM 19/2/2015 at FAO Rome
## Last updated on 5/3/2015 by JAM

## remove before uploading to SWS
## rm(list = ls())

## Load required functions
library(data.table)
library(faosws)
library(dplyr)
library(reshape2)

TEST_MODE=FALSE

if(!exists("DEBUG_MODE") || DEBUG_MODE)  {
    token = "bdcbefca-0dfe-4dd5-aeeb-3e8bb36dbd4d"
    GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws",token)
    TEST_MODE=TRUE
}

## shows a list of parameters that have been set within the SWS by user at
## startup
swsContext.computationParams

## set the keys to get the tourist data from the FAO working system
destinationAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
    "destinationCountryM49")
originAreaCodes <- faosws::GetCodeList("tourism", "tourist_flow",
    "originCountryM49")
tourismElementCodes <- faosws::GetCodeList("tourism", "tourist_consumption",
    "tourismElement")

## set the year range to pull data from the SWS
yearRange <- as.character(2009:2011)
## yearRange <- as.character(swsContext.computationParams$year_range)

##Pull the bidirectional movement data from SWS pertaining to tourist visitors
##to all countries
dim1 <- Dimension(name = "destinationCountryM49", keys =
    destinationAreaCodes[ , code] )
dim2 <- Dimension(name = "originCountryM49", keys = originAreaCodes [, code] )
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
data3 <- as.data.table (dcast(data2, dest + year ~ element))

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
data4 <- merge(data1, data3, by=c("dest", "year"))

## rearrange the column order to make it easier to view
data4 <- setcolorder(data4, neworder = c("year", "orig", "dest", "onVisNum",
                                "onVisDays", "totDayVisNum"))

## a small number of countries are missing values for overnight visitor days,
## "onVisDays" and this affects the bi-directional calculations for them, but
## also all of the other countries as well, so this imputes the missing number
## of days, by taking the mean of all day numbers present, grouped by year.
data4[ , onVisDays := ifelse (is.na (onVisDays), round (mean
                       (onVisDays, na.rm=TRUE), 2), onVisDays), by = year]

## calculate the total number tourist visitor days, the product of overnight
## visitor number and days per visit
data4[ , onVisTotDays := onVisNum * onVisDays]

## set the keys for the data.table to sort by year and country of origin
setkey(data4, year, orig, dest)

## calculate a new total overnight visitor number per country of destination, to
## be used later to proportion the day visitor number, because we do not have
## data for country of origin, and allocate them to a country of origin,
## assuming they arrive in the same relative proportions as the overnight
## visitors
data5 <- data4[ , totOnVisNum := sum(onVisNum), by=list(year,dest)]

## do the proportioning of the total day visitor number, based on the
## proportions of overnight visitors that originated from each country
data5 <- data5[ , proDayVisNum := round (onVisNum / totOnVisNum * totDayVisNum, 1)]

## create a new total visitor days by summing the overnight viistor days, and
## the day visitor days
data5 <- data5[ , totVisDays := onVisTotDays + proDayVisNum]

## calculate the number of calories consumed by all tourists as the product of
## number of days and average tourist consumption in calories, which was input
## as a parameter
data5 <- data5[ , totVisCals := totVisDays * as.numeric
               (swsContext.computationParams$tourist_consumption) ]

## set the keys to get the calorie consuption, by individual FBS commodity for
## each country from the FAO working system
foodAreaCodes <- faosws::GetCodeList("suafbs", "fbs", "geographicAreaM49")
foodElementCodes <- faosws::GetCodeList("suafbs", "fbs", "measuredElementSuaFbs")
## the Item codes require subsetting to the get the 148 codes that correspond to
## the FBS calculations
foodItemCodes <- faosws::GetCodeList("suafbs", "fbs",
                                     "measuredItemSuaFbs")[grep("^S", code)]

##Pull the supply utilization account(SUA) food balance sheet (FBS) data from
##SWS pertaining to calorie consumption from each commodity in each country
dim1 <- Dimension(name = "geographicAreaM49", keys = foodAreaCodes[ , code] )
dim2 <- Dimension(name = "measuredElementSuaFbs", keys = ("664"))
dim3 <- Dimension(name = "measuredItemSuaFbs", keys = foodItemCodes[, code] )
dim4 <- Dimension(name = "timePointYears", keys = yearRange)
key <- DatasetKey(domain = "suafbs", dataset = "fbs",
                  dimensions = list(dim1, dim2, dim3, dim4))

## download the calorie consumption data from the SWS
data6 <- GetData(key, flags = FALSE)

## remove the measuredElement column which is of no value to me here
data6 <- data6[, which(!grepl("Element", colnames(data6))), with=FALSE]

## set the column names to small simple ones representing destination, database
## element, year and value
setnames(data6, old = colnames(data6), new = c("orig", "item", "year", "calValue"))

## rearrange the column order to match previous data.tables
data6 <- setcolorder(data6, neworder = c("year", "orig", "item", "calValue"))

## calculate the total calories consumed, per day, by each country as the sum of
## all individual commodity calories, based on the supply utilization account -
## food balance sheet data
data6[ , totalCaloriesByOrigSuaFbs := sum(calValue), by = list (orig, year)]

## calculate the proportional calories consumed, by Item, per day, as Item
## calories divided by the total for the whole country
data6[ , propCaloriesByItemSuaFbs := calValue / totalCaloriesByOrigSuaFbs]

## calculate total calories consumed by Item, per day by the tourist in the
## country they are visiting, based on proportions of what they eat at home
data6[ , totCaloriesByItemPerDay :=
      as.numeric(swsContext.computationParams$tourist_consumption) *
      propCaloriesByItemSuaFbs]

## set the keys for the data.table to sort by year and country of origin
setkey(data6, year, orig, item)

## merge data5 and data6 to allow calculation of calories by commodity
data7 <- merge(data5,data6, allow.cartesian=TRUE)

## calculate the total calories consumed, by item, for the entire year
data7[ , totCaloriesByItemPerYear := totVisCals * propCaloriesByItemSuaFbs]

## calculate calories consumed within a country, by tourists visiting from other
## countries, and therefore these calories can be subtracted from the total
## calories available to the permanent population in the country listed in
## column #3
touristTotalCaloriesInCountry <- data5[, list(calsVisToCountry = sum(totVisCals)),
                                  by = list(year,dest)]

## calculate calories by people that left the country to be a tourist in the
## destination country, therefore these calories can be added to the total
## calories available to the permanent population country of origin, which is
## listed in column
touristTotalCaloriesOutCountry <- data5[, list(calsVisFromCountry = sum(totVisCals)),
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
## to make sure they corroborate with the originals in data5
crosscheck <- data7[, sum(totCaloriesByItemPerYear), by = list (dest, orig, year)]

## assemble two outputs into a list to be returned
touristCalories <- list(touristTotalCaloriesInCountry,
                        touristTotalCaloriesOutCountry)

## assemble two outputs into a list to be returned
touristCaloriesByItem <- list(touristCaloriesInCountryByItem,
                              touristCaloriesOutCountryByItem)

## SWS will not exit unless it passes a small message of <1000 characters back
## to the screen so this allows it to complete when testing on the SWS
print("I've finished")
