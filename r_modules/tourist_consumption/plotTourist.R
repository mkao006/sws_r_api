## This script is designed to plot the proportion of calories consumed by
## tourists in comparison to the calories consumed locally.  This file should
## NOT be zipped into the R module.  It is used after data7 has been created in
## the bidirectionTourist.R file.

library(ggplot2)
library(maps)

## Load data
load("~/Documents/Github/sws_r_api/r_modules/tourist_consumption/processedData.RData")
countryMap = fread("~/Documents/Github/sws_r_api/r_modules/tourist_consumption/m49Names.csv")
countryMap[, M49 := as.character(M49)]

## Merge orig to country name
setkeyv(data7, "orig")
setnames(countryMap, "M49", "orig")
data7 = merge(data7, countryMap)
setnames(data7, "Country", "origCountry")
data7[, CountryAbv := NULL]

## Merge dest to country name
setkeyv(data7, "dest")
setnames(countryMap, "orig", "dest")
data7 = merge(data7, countryMap)
setnames(data7, "Country", "destCountry")
data7[, CountryAbv := NULL]

## Generate summary data
lostCalories = data7[, list(lostCalories = sum(totVisDays*calValue)),
                      by = "destCountry"]
gainedCalories = data7[, list(gainedCalories = sum(totVisDays*calValue)),
                        by = "origCountry"]
localCalories = data7[, list(localCalories = mean(totalLocalCalories * population)),
                       by = "origCountry"]
summaryTable = merge.data.frame(lostCalories, localCalories,
                                by.x = "destCountry", by.y = "origCountry", all = TRUE)
summaryTable = merge.data.frame(summaryTable, gainedCalories,
                                by.x = "destCountry", by.y = "origCountry")
colnames(summaryTable)[1] = "Country"

## Create country data object
plotObject = map_data("world")
plotObject = merge(plotObject, summaryTable, by.x = "region", by.y = "Country",
                   all.x = TRUE)
plotObject = plotObject[order(plotObject$order), ]

ggplot(plotObject, aes(x = long, y = lat, group = group,
                       fill = ifelse(is.na(lostCalories), 0, lostCalories))) +
    geom_polygon()

ggplot(plotObject, aes(x = long, y = lat, group = group,
                       fill = lostCalories)) +
    geom_polygon()

ggplot(plotObject, aes(x = long, y = lat, group = group,
                       fill = lostCalories/localCalories)) +
    geom_polygon()
