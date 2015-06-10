library(data.table)
library(faosws)
library(dplyr)
#library(reshape2)

TEST_MODE=FALSE

if(!exists("DEBUG_MODE") || DEBUG_MODE) {
    token = "c22239d3-974e-4bf8-ac53-cb0f9a4cb525"
    GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws",token)
    TEST_MODE=TRUE
    if(Sys.info()[7] == "josh")
        source("~/Documents/Github/sws_r_api/r_modules/food_imputation/R/FoodModule.R")
    if(Sys.info()[7] == "")
        source("R/FoodModule.R")
}

## set the keys to get the population data from the FAO working system
areaCodes = faosws::GetCodeList("population", "population",
    "geographicAreaM49")

populationCodes = faosws::GetCodeList("population", "population",
    "measuredElementPopulation")

dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[which(areaCodes[,type]=="country"),]$code) # only type==country
dim2 = Dimension(name = "measuredElementPopulation", keys = "21")       # Food balance sheet population
years = swsContext.datasets[[1]]@dimensions$timePointYears@keys
dim3 = Dimension(name = "timePointYears", keys = c(as.character(2007:2013)))
key = DatasetKey(domain = "population", dataset = "population",
                  dimensions = list(dim1, dim2, dim3))


## download the population data from the SWS
popData <- GetData(key, flags=FALSE, normalized = FALSE)

## change names of columns (variables) to something more logical
setnames(popData, c(names(popData)),
         c("M49_cod", "pop_type", "pop_2007", "pop_2008", "pop_2009","pop_2010","pop_2011", "pop_2012", "pop_2013"))

popData[,  M49_cod := as.numeric (M49_cod)]


## set the keys to get the gross domestic product (gdp) data from the FAO
## working system
areaCodes = faosws::GetCodeList("WorldBank", "wb_ecogrw",
    "geographicAreaM49")
indicatorCodes = faosws::GetCodeList("WorldBank", "wb_ecogrw",
    "wbIndicator")

dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[which(areaCodes[,type]=="country"),]$code) # only type==country
dim2 = Dimension(name = "wbIndicator", keys = "NY.GDP.PCAP.KD") # GDP per capita (constant 2500 US$)
dim3 = Dimension(name = "timePointYears", keys = c(as.character(2007:2013)))
key = DatasetKey(domain = "WorldBank", dataset = "wb_ecogrw",
                  dimensions = list(dim1, dim2, dim3))

## download the gdp data from the SWS
gdpData <- GetData(key, flags=FALSE, normalized = FALSE)

## change names of columns (variables) to something more logical
setnames(gdpData, c(names(gdpData)),
         c("M49_cod", "WBInd", "gdp_pc_2007", "gdp_pc_2008","gdp_pc_2009","gdp_pc_2010", "gdp_pc_2011", "gdp_pc_2012","gdp_pc_2013"))

gdpData[,  M49_cod := as.numeric (M49_cod)]

## merge the current population and gross domestic product data into a single
## file

GdpPopData <- merge(popData, gdpData, by = "M49_cod", all = TRUE)
#combineData1[ , c("elementNum", "flag", "indicator", "year.x", "year.y") := NULL]

GdpPopData <- as.data.frame(GdpPopData)[,-c(2,10)] 
head(GdpPopData)

areaCodes = faosws::GetCodeList("faostat_one","FS1_SUA_UPD","geographicAreaFS")
foodCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredElementFS")
suaCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")

dim1 = Dimension(name = "geographicAreaFS", keys = areaCodes[,code]) 
dim2 = Dimension(name = "measuredElementFS", keys = c("141","264","274","284")) # food, cal/caput/day, prot/caput/day, fats/caput/day
dim3 = Dimension(name = "measuredItemFS", keys = as.character(suaCodes[, code]))
dim4 = Dimension(name = "timePointYears", keys = c(as.character(2007)))
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                 dimensions = list(dim1, dim2, dim3, dim4))


pivot1 = Pivoting(code = "geographicAreaFS", ascending = TRUE)
pivot2 = Pivoting(code = "measuredElementFS", ascending = TRUE)
pivot3 = Pivoting(code = "measuredItemFS", ascending = TRUE)
pivot4 = Pivoting(code = "timePointYears", ascending = FALSE)

## download the food data from the SWS
foodData_2007 <- GetData(key,flags=FALSE, normalized = FALSE, pivoting = c(pivot1, pivot3, pivot4, pivot2))

## change names of columns (variables) to something more logical
setnames(foodData_2007, c(names(foodData_2007)),
         c("country_cod", "com_sua_cod", "year", "food_2007", "cal_pc_2007", "prot_pc_2007", "fat_pc_2007"))

# str(foodData_2007)
foodData_2007[,  country_cod := as.numeric (country_cod)]

## set the keys to get the Food data from the FAO working system
areaCodes = faosws::GetCodeList("faostat_one","FS1_SUA_UPD","geographicAreaFS")
foodCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredElementFS")
suaCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")

dim1 = Dimension(name = "geographicAreaFS", keys = areaCodes[,code]) 
dim2 = Dimension(name = "measuredElementFS", keys = c("141","264","274","284")) # food, cal/caput/day, prot/caput/day, fats/caput/day
dim3 = Dimension(name = "measuredItemFS", keys = as.character(suaCodes[, code]))
dim4 = Dimension(name = "timePointYears", keys = c(as.character(2008)))
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                 dimensions = list(dim1, dim2, dim3, dim4))


pivot1 = Pivoting(code = "geographicAreaFS", ascending = TRUE)
pivot2 = Pivoting(code = "measuredElementFS", ascending = TRUE)
pivot3 = Pivoting(code = "measuredItemFS", ascending = TRUE)
pivot4 = Pivoting(code = "timePointYears", ascending = FALSE)

## download the food data from the SWS
foodData_2008 <- GetData(key,flags=FALSE, normalized = FALSE, pivoting = c(pivot1, pivot3, pivot4, pivot2))

## change names of columns (variables) to something more logical
setnames(foodData_2008, c(names(foodData_2008)),
         c("country_cod", "com_sua_cod", "year", "food_2008", "cal_pc_2008", "prot_pc_2008", "fat_pc_2008"))

foodData_2008[,  country_cod := as.numeric (country_cod)]

## set the keys to sort the food data file
setkey(foodData_2008, country_cod)



## set the keys to get the Food data from the FAO working system
areaCodes = faosws::GetCodeList("faostat_one","FS1_SUA_UPD","geographicAreaFS")
foodCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredElementFS")
suaCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")

dim1 = Dimension(name = "geographicAreaFS", keys = areaCodes[,code]) 
dim2 = Dimension(name = "measuredElementFS", keys = c("141","264","274","284")) # food, cal/caput/day, prot/caput/day, fats/caput/day
dim3 = Dimension(name = "measuredItemFS", keys = as.character(suaCodes[, code]))
dim4 = Dimension(name = "timePointYears", keys = c(as.character(2009)))
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                 dimensions = list(dim1, dim2, dim3, dim4))


pivot1 = Pivoting(code = "geographicAreaFS", ascending = TRUE)
pivot2 = Pivoting(code = "measuredElementFS", ascending = TRUE)
pivot3 = Pivoting(code = "measuredItemFS", ascending = TRUE)
pivot4 = Pivoting(code = "timePointYears", ascending = FALSE)

## download the food data from the SWS
foodData_2009 <- GetData(key,flags=FALSE, normalized = FALSE, pivoting = c(pivot1, pivot3, pivot4, pivot2))

## change names of columns (variables) to something more logical
setnames(foodData_2009, c(names(foodData_2009)),
         c("country_cod", "com_sua_cod", "year", "food_2009", "cal_pc_2009", "prot_pc_2009", "fat_pc_2009"))

foodData_2009[,  country_cod := as.numeric (country_cod)]


## set the keys to get the Food data from the FAO working system
areaCodes = faosws::GetCodeList("faostat_one","FS1_SUA_UPD","geographicAreaFS")
foodCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredElementFS")
suaCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")

dim1 = Dimension(name = "geographicAreaFS", keys = areaCodes[,code]) 
dim2 = Dimension(name = "measuredElementFS", keys = c("141","264","274","284")) # food, cal/caput/day, prot/caput/day, fats/caput/day
dim3 = Dimension(name = "measuredItemFS", keys = as.character(suaCodes[, code]))
dim4 = Dimension(name = "timePointYears", keys = c(as.character(2010)))
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                 dimensions = list(dim1, dim2, dim3, dim4))


pivot1 = Pivoting(code = "geographicAreaFS", ascending = TRUE)
pivot2 = Pivoting(code = "measuredElementFS", ascending = TRUE)
pivot3 = Pivoting(code = "measuredItemFS", ascending = TRUE)
pivot4 = Pivoting(code = "timePointYears", ascending = FALSE)

## download the food data from the SWS
foodData_2010 <- GetData(key,flags=FALSE, normalized = FALSE, pivoting = c(pivot1, pivot3, pivot4, pivot2))

## change names of columns (variables) to something more logical
setnames(foodData_2010, c(names(foodData_2010)),
         c("country_cod", "com_sua_cod", "year", "food_2010", "cal_pc_2010", "prot_pc_2010", "fat_pc_2010"))

foodData_2010[,  country_cod := as.numeric (country_cod)]



## set the keys to get the Food data from the FAO working system
areaCodes = faosws::GetCodeList("faostat_one","FS1_SUA_UPD","geographicAreaFS")
foodCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredElementFS")
suaCodes = faosws::GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS")

dim1 = Dimension(name = "geographicAreaFS", keys = areaCodes[,code]) 
dim2 = Dimension(name = "measuredElementFS", keys = c("141","264","274","284")) # food, cal/caput/day, prot/caput/day, fats/caput/day
dim3 = Dimension(name = "measuredItemFS", keys = as.character(suaCodes[, code]))
dim4 = Dimension(name = "timePointYears", keys = c(as.character(2011)))
key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA_UPD",
                 dimensions = list(dim1, dim2, dim3, dim4))


pivot1 = Pivoting(code = "geographicAreaFS", ascending = TRUE)
pivot2 = Pivoting(code = "measuredElementFS", ascending = TRUE)
pivot3 = Pivoting(code = "measuredItemFS", ascending = TRUE)
pivot4 = Pivoting(code = "timePointYears", ascending = FALSE)

## download the food data from the SWS
foodData_2011 <- GetData(key,flags=FALSE, normalized = FALSE, pivoting = c(pivot1, pivot3, pivot4, pivot2))

## change names of columns (variables) to something more logical
setnames(foodData_2011, c(names(foodData_2011)),
         c("country_cod", "com_sua_cod", "year", "food_2011", "cal_pc_2011", "prot_pc_2011", "fat_pc_2011"))

foodData_2011[,  country_cod := as.numeric (country_cod)]



foodData_2007 <- as.data.frame(foodData_2007)[,-3]

foodData_2008 <- as.data.frame(foodData_2008)[,-3]

foodData_2007_2008 <- merge(foodData_2007, foodData_2008,  
                                    by.y = c("country_cod","com_sua_cod"), 
                                    by.x = c("country_cod","com_sua_cod"),
                                    all = TRUE)

foodData_2009 <- as.data.frame(foodData_2009)[,-3]

foodData_2007_2009 <- merge(foodData_2007_2008, foodData_2009,  
                            by.y = c("country_cod","com_sua_cod"), 
                            by.x = c("country_cod","com_sua_cod"),
                            all = TRUE)


foodData_2010 <- as.data.frame(foodData_2010)[,-3]

foodData_2007_2010 <- merge(foodData_2007_2009, foodData_2010,  
                            by.y = c("country_cod","com_sua_cod"), 
                            by.x = c("country_cod","com_sua_cod"),
                            all = TRUE)

foodData_2011 <- as.data.frame(foodData_2011)[,-3]

foodData <- merge(foodData_2007_2010, foodData_2011,  
                            by.y = c("country_cod","com_sua_cod"), 
                            by.x = c("country_cod","com_sua_cod"),
                            all = TRUE)


foodData$com_cod <-  rep(NA,length(foodData$com_sua_cod))
foodData$fdm_cod <- rep(NA,length(foodData$com_sua_cod))



for(i in 1:dim(foodData)[1]){
  
  
  if(foodData$com_sua_cod[i]=='15' | foodData$com_sua_cod[i]=='16'  | foodData$com_sua_cod[i]=='17' |
       foodData$com_sua_cod[i]=='18' | foodData$com_sua_cod[i]=='19'| foodData$com_sua_cod[i]=='20' |
       foodData$com_sua_cod[i]=='21'| foodData$com_sua_cod[i]=='22'| foodData$com_sua_cod[i]=='23' |
       foodData$com_sua_cod[i]=='24'| foodData$com_sua_cod[i]=='41'| foodData$com_sua_cod[i]=='110' |
       foodData$com_sua_cod[i]=='114'| foodData$com_sua_cod[i]=='115') {
    
    foodData$fdm_cod[i] <- 1
    foodData$com_cod[i] <- 2511
  }
  
  else if (foodData$com_sua_cod[i]=='44' | foodData$com_sua_cod[i]=='45'  | foodData$com_sua_cod[i]=='46' |
             foodData$com_sua_cod[i]=='47' | foodData$com_sua_cod[i]=='48'| foodData$com_sua_cod[i]=='49' |
             foodData$com_sua_cod[i]=='50') {
    
    foodData$fdm_cod[i] <- 4
    foodData$com_cod[i] <- 2513
  }
  
  
  else if (foodData$com_sua_cod[i]=='56' | foodData$com_sua_cod[i]=='57'  | foodData$com_sua_cod[i]=='58' |
             foodData$com_sua_cod[i]=='59' | foodData$com_sua_cod[i]=='63'| foodData$com_sua_cod[i]=='64' |
             foodData$com_sua_cod[i]=='846') {
    
    foodData$fdm_cod[i] <- 3
    foodData$com_cod[i] <- 2514
  }
  
  
  else if (foodData$com_sua_cod[i]=='71' | foodData$com_sua_cod[i]=='72'  | foodData$com_sua_cod[i]=='73') {
    
    foodData$fdm_cod[i] <- 8
    foodData$com_cod[i] <- 3005
  }
  
  else if (foodData$com_sua_cod[i]=='75' | foodData$com_sua_cod[i]=='76'  | foodData$com_sua_cod[i]=='77') {
    
    foodData$fdm_cod[i] <- 5
    foodData$com_cod[i] <- 2516
  }
  
  else if (foodData$com_sua_cod[i]=='79' | foodData$com_sua_cod[i]=='80'  | foodData$com_sua_cod[i]=='81') {
    
    foodData$fdm_cod[i] <- 6
    foodData$com_cod[i] <- 2517
    
  }
  
  else if (foodData$com_sua_cod[i]=='83' | foodData$com_sua_cod[i]=='84'  | foodData$com_sua_cod[i]=='85') {
    
    foodData$fdm_cod[i] <- 7
    foodData$com_cod[i] <- 2518
  }
  
  
  else if (foodData$com_sua_cod[i]=='68' | foodData$com_sua_cod[i]=='89'  | foodData$com_sua_cod[i]=='90' |
             foodData$com_sua_cod[i]=='91' | foodData$com_sua_cod[i]=='92'| foodData$com_sua_cod[i]=='94' |
             foodData$com_sua_cod[i]=='95' | foodData$com_sua_cod[i]=='96'  | foodData$com_sua_cod[i]=='97' |
             foodData$com_sua_cod[i]=='98' | foodData$com_sua_cod[i]=='99'| foodData$com_sua_cod[i]=='101' |
             foodData$com_sua_cod[i]=='103' | foodData$com_sua_cod[i]=='104'  | foodData$com_sua_cod[i]=='105' |
             foodData$com_sua_cod[i]=='108' | foodData$com_sua_cod[i]=='111'| foodData$com_sua_cod[i]=='112' |
             foodData$com_sua_cod[i]=='113') {
    
    foodData$fdm_cod[i] <- 8
    foodData$com_cod[i] <- 3005
  }
  
  
  else if (foodData$com_sua_cod[i]=='116' | foodData$com_sua_cod[i]=='117'  | foodData$com_sua_cod[i]=='118' |
             foodData$com_sua_cod[i]=='119' | foodData$com_sua_cod[i]=='121') {
    
    foodData$fdm_cod[i] <- 9
    foodData$com_cod[i] <- 2531
  }
  
  else if (foodData$com_sua_cod[i]=='125' | foodData$com_sua_cod[i]=='126'  | foodData$com_sua_cod[i]=='127' |
             foodData$com_sua_cod[i]=='128' | foodData$com_sua_cod[i]=='129') {
    
    foodData$fdm_cod[i] <- 11
    foodData$com_cod[i] <- 2532
  }
  
  else if (foodData$com_sua_cod[i]=='122') {
    
    foodData$fdm_cod[i] <- 10
    foodData$com_cod[i] <- 2533
  }
  
  else if (foodData$com_sua_cod[i]=='135' | foodData$com_sua_cod[i]=='136'  | foodData$com_sua_cod[i]=='149' |
             foodData$com_sua_cod[i]=='150' | foodData$com_sua_cod[i]=='151') {
    
    foodData$fdm_cod[i] <- 14
    foodData$com_cod[i] <- 2534
  }
  
  else if (foodData$com_sua_cod[i]=='137') {
    
    foodData$fdm_cod[i] <- 12
    foodData$com_cod[i] <- 2535
  }
  
  else if (foodData$com_sua_cod[i]=='156' | foodData$com_sua_cod[i]=='157') {
    
    foodData$fdm_cod[i] <- 17
    foodData$com_cod[i] <- 3006
  }
  
  else if (foodData$com_sua_cod[i]=='163') {
    
    foodData$fdm_cod[i] <- 16
    foodData$com_cod[i] <- 2541
  }
  
  else if (foodData$com_sua_cod[i]=='158' | foodData$com_sua_cod[i]=='159'  | foodData$com_sua_cod[i]=='162' |
             foodData$com_sua_cod[i]=='164' | foodData$com_sua_cod[i]=='168' | foodData$com_sua_cod[i]=='171') {
    
    foodData$fdm_cod[i] <- 15
    foodData$com_cod[i] <- 2542
  }
  
  else if (foodData$com_sua_cod[i]=='154' | foodData$com_sua_cod[i]=='155'  | foodData$com_sua_cod[i]=='160' |
             foodData$com_sua_cod[i]=='161' | foodData$com_sua_cod[i]=='165'| foodData$com_sua_cod[i]=='166' |
             foodData$com_sua_cod[i]=='167' | foodData$com_sua_cod[i]=='172'  | foodData$com_sua_cod[i]=='173' |
             foodData$com_sua_cod[i]=='175' | foodData$com_sua_cod[i]=='633'  | foodData$com_sua_cod[i]=='1182') {
    
    foodData$fdm_cod[i] <- 17
    foodData$com_cod[i] <- 3006
  }  
  
  else if (foodData$com_sua_cod[i]=='176' | foodData$com_sua_cod[i]=='187'  | foodData$com_sua_cod[i]=='181' |
             foodData$com_sua_cod[i]=='191' | foodData$com_sua_cod[i]=='195'| foodData$com_sua_cod[i]=='197' |
             foodData$com_sua_cod[i]=='201' | foodData$com_sua_cod[i]=='203'  | foodData$com_sua_cod[i]=='205' |
             foodData$com_sua_cod[i]=='210' | foodData$com_sua_cod[i]=='211'| foodData$com_sua_cod[i]=='212' |
             foodData$com_sua_cod[i]=='213') {
    
    foodData$fdm_cod[i] <- 18
    foodData$com_cod[i] <- 2911
  }  
  
  else if (foodData$com_sua_cod[i]=='216' | foodData$com_sua_cod[i]=='217'  | foodData$com_sua_cod[i]=='220' |
             foodData$com_sua_cod[i]=='221' | foodData$com_sua_cod[i]=='222'| foodData$com_sua_cod[i]=='223' |
             foodData$com_sua_cod[i]=='224' | foodData$com_sua_cod[i]=='225'  | foodData$com_sua_cod[i]=='226' |
             foodData$com_sua_cod[i]=='229' | foodData$com_sua_cod[i]=='230'| foodData$com_sua_cod[i]=='231' |
             foodData$com_sua_cod[i]=='232' | foodData$com_sua_cod[i]=='233'| foodData$com_sua_cod[i]=='234' |
             foodData$com_sua_cod[i]=='235') {
    
    foodData$fdm_cod[i] <- 19
    foodData$com_cod[i] <- 2912
  }  
  
  else if (foodData$com_sua_cod[i]=='236' | foodData$com_sua_cod[i]=='239'  | foodData$com_sua_cod[i]=='240' |
             foodData$com_sua_cod[i]=='241' |
             foodData$com_sua_cod[i]=='242' | foodData$com_sua_cod[i]=='243'  | foodData$com_sua_cod[i]=='246' |
             foodData$com_sua_cod[i]=='247' |
             foodData$com_sua_cod[i]=='267' | foodData$com_sua_cod[i]=='270'  | foodData$com_sua_cod[i]=='292' |
             foodData$com_sua_cod[i]=='295' |
             foodData$com_sua_cod[i]=='329' | foodData$com_sua_cod[i]=='249'  | foodData$com_sua_cod[i]=='250' |
             foodData$com_sua_cod[i]=='251'|
             foodData$com_sua_cod[i]=='289' | foodData$com_sua_cod[i]=='254'  | foodData$com_sua_cod[i]=='256' |
             foodData$com_sua_cod[i]=='260'|
             foodData$com_sua_cod[i]=='262' | foodData$com_sua_cod[i]=='263'  | foodData$com_sua_cod[i]=='265' |
             foodData$com_sua_cod[i]=='275'|
             foodData$com_sua_cod[i]=='277' | foodData$com_sua_cod[i]=='280'  | foodData$com_sua_cod[i]=='296' |
             foodData$com_sua_cod[i]=='299'|
             foodData$com_sua_cod[i]=='305' | foodData$com_sua_cod[i]=='310'  | foodData$com_sua_cod[i]=='311' |
             foodData$com_sua_cod[i]=='312'|
             foodData$com_sua_cod[i]=='333' | foodData$com_sua_cod[i]=='336'  | foodData$com_sua_cod[i]=='339' |
             foodData$com_sua_cod[i]=='343') {
    
    foodData$fdm_cod[i] <- 20
    foodData$com_cod[i] <- 2913
  }
  
  else if (foodData$com_sua_cod[i]=='237' | foodData$com_sua_cod[i]=='44'  | foodData$com_sua_cod[i]=='268' |
             foodData$com_sua_cod[i]=='271' |
             foodData$com_sua_cod[i]=='293' | foodData$com_sua_cod[i]=='331'  | foodData$com_sua_cod[i]=='258' |
             foodData$com_sua_cod[i]=='257' |
             foodData$com_sua_cod[i]=='1276' | foodData$com_sua_cod[i]=='1277'  | foodData$com_sua_cod[i]=='252' |
             foodData$com_sua_cod[i]=='290' |
             foodData$com_sua_cod[i]=='261' | foodData$com_sua_cod[i]=='274'  | foodData$com_sua_cod[i]=='36' |
             foodData$com_sua_cod[i]=='60'|
             foodData$com_sua_cod[i]=='264' | foodData$com_sua_cod[i]=='266'  | foodData$com_sua_cod[i]=='276' |
             foodData$com_sua_cod[i]=='278'|
             foodData$com_sua_cod[i]=='281' | foodData$com_sua_cod[i]=='297'  | foodData$com_sua_cod[i]=='306' |
             foodData$com_sua_cod[i]=='307'|
             foodData$com_sua_cod[i]=='313' | foodData$com_sua_cod[i]=='334'  | foodData$com_sua_cod[i]=='337' |
             foodData$com_sua_cod[i]=='340'|
             foodData$com_sua_cod[i]=='664' | foodData$com_sua_cod[i]=='1241'  | foodData$com_sua_cod[i]=='1242' |
             foodData$com_sua_cod[i]=='1273'|
             foodData$com_sua_cod[i]=='1274' | foodData$com_sua_cod[i]=='1275') {
    
    foodData$fdm_cod[i] <- 40
    foodData$com_cod[i] <- 2914
  }
  
  
  else if (foodData$com_sua_cod[i]=='388' | foodData$com_sua_cod[i]=='389'  | foodData$com_sua_cod[i]=='390' |
             foodData$com_sua_cod[i]=='391' |
             foodData$com_sua_cod[i]=='392' | foodData$com_sua_cod[i]=='403'  | foodData$com_sua_cod[i]=='358' |
             foodData$com_sua_cod[i]=='366' |
             foodData$com_sua_cod[i]=='367' | foodData$com_sua_cod[i]=='372'  | foodData$com_sua_cod[i]=='373' |
             foodData$com_sua_cod[i]=='378' |
             foodData$com_sua_cod[i]=='393' | foodData$com_sua_cod[i]=='394'  | foodData$com_sua_cod[i]=='397' |
             foodData$com_sua_cod[i]=='399'|
             foodData$com_sua_cod[i]=='401' | foodData$com_sua_cod[i]=='402'  | foodData$com_sua_cod[i]=='406' |
             foodData$com_sua_cod[i]=='407'|
             foodData$com_sua_cod[i]=='414' | foodData$com_sua_cod[i]=='417'  | foodData$com_sua_cod[i]=='420' |
             foodData$com_sua_cod[i]=='423'|
             foodData$com_sua_cod[i]=='426' | foodData$com_sua_cod[i]=='430'  | foodData$com_sua_cod[i]=='446' |
             foodData$com_sua_cod[i]=='430'|
             foodData$com_sua_cod[i]=='446' | foodData$com_sua_cod[i]=='447'  | foodData$com_sua_cod[i]=='448' |
             foodData$com_sua_cod[i]=='449'|
             foodData$com_sua_cod[i]=='450' | foodData$com_sua_cod[i]=='451' |
             foodData$com_sua_cod[i]=='459' | foodData$com_sua_cod[i]=='461'  | foodData$com_sua_cod[i]=='463' |
             foodData$com_sua_cod[i]=='464'|
             foodData$com_sua_cod[i]=='465' | foodData$com_sua_cod[i]=='466'  | foodData$com_sua_cod[i]=='469' |
             foodData$com_sua_cod[i]=='471'|
             foodData$com_sua_cod[i]=='472' | foodData$com_sua_cod[i]=='473'  | foodData$com_sua_cod[i]=='474' |
             foodData$com_sua_cod[i]=='475' | foodData$com_sua_cod[i]=='476'  | foodData$com_sua_cod[i]=='658') {
    
    foodData$fdm_cod[i] <- 21
    foodData$com_cod[i] <- 2918
  }
  
  
  else if (foodData$com_sua_cod[i]=='490' | foodData$com_sua_cod[i]=='491'  | foodData$com_sua_cod[i]=='492' |
             foodData$com_sua_cod[i]=='495' | foodData$com_sua_cod[i]=='496') {
    
    foodData$fdm_cod[i] <- 22
    foodData$com_cod[i] <- 2611
  }    
  
  
  else if (foodData$com_sua_cod[i]=='497' | foodData$com_sua_cod[i]=='498'  | foodData$com_sua_cod[i]=='499') {
    
    foodData$fdm_cod[i] <- 23
    foodData$com_cod[i] <- 2612
  }    
  
  
  else if (foodData$com_sua_cod[i]=='507' | foodData$com_sua_cod[i]=='509'  | foodData$com_sua_cod[i]=='510' |
             foodData$com_sua_cod[i]=='512' | foodData$com_sua_cod[i]=='513'  | foodData$com_sua_cod[i]=='514' ) {
    
    foodData$fdm_cod[i] <- 24
    foodData$com_cod[i] <- 3000
  } 
  
  else if (foodData$com_sua_cod[i]=='486' ) {
    
    foodData$fdm_cod[i] <- 25
    foodData$com_cod[i] <- 2615
  } 
  
  else if (foodData$com_sua_cod[i]=='489' ) {
    
    foodData$fdm_cod[i] <- 13
    foodData$com_cod[i] <- 2616
  }
  
  else if (foodData$com_sua_cod[i]=='574' | foodData$com_sua_cod[i]=='575'  | foodData$com_sua_cod[i]=='576' |
             foodData$com_sua_cod[i]=='580' |
             foodData$com_sua_cod[i]=='577' | foodData$com_sua_cod[i]=='560'  | foodData$com_sua_cod[i]=='561' |
             foodData$com_sua_cod[i]=='562' |
             foodData$com_sua_cod[i]=='563' | foodData$com_sua_cod[i]=='521'  | foodData$com_sua_cod[i]=='523' |
             foodData$com_sua_cod[i]=='526' |
             foodData$com_sua_cod[i]=='527' | foodData$com_sua_cod[i]=='530'  | foodData$com_sua_cod[i]=='531' |
             foodData$com_sua_cod[i]=='534'|
             foodData$com_sua_cod[i]=='536' | foodData$com_sua_cod[i]=='537'  | foodData$com_sua_cod[i]=='538' |
             foodData$com_sua_cod[i]=='539'|
             foodData$com_sua_cod[i]=='541' | foodData$com_sua_cod[i]=='542'  | foodData$com_sua_cod[i]=='544' |
             foodData$com_sua_cod[i]=='547'|
             foodData$com_sua_cod[i]=='538' | foodData$com_sua_cod[i]=='539'  | foodData$com_sua_cod[i]=='541' |
             foodData$com_sua_cod[i]=='542'|
             foodData$com_sua_cod[i]=='544' | foodData$com_sua_cod[i]=='547'  | foodData$com_sua_cod[i]=='549' |
             foodData$com_sua_cod[i]=='550'|
             foodData$com_sua_cod[i]=='552' | foodData$com_sua_cod[i]=='554' |
             foodData$com_sua_cod[i]=='558' | foodData$com_sua_cod[i]=='567'  | foodData$com_sua_cod[i]=='568' |
             foodData$com_sua_cod[i]=='569'|
             foodData$com_sua_cod[i]=='570' | foodData$com_sua_cod[i]=='571'  | foodData$com_sua_cod[i]=='572' |
             foodData$com_sua_cod[i]=='583'|
             foodData$com_sua_cod[i]=='587' | foodData$com_sua_cod[i]=='591'  | foodData$com_sua_cod[i]=='592' |
             foodData$com_sua_cod[i]=='600' | foodData$com_sua_cod[i]=='603'  | foodData$com_sua_cod[i]=='604' |
             foodData$com_sua_cod[i]=='619' | foodData$com_sua_cod[i]=='620'  | foodData$com_sua_cod[i]=='622' |
             foodData$com_sua_cod[i]=='623' | foodData$com_sua_cod[i]=='624'  | foodData$com_sua_cod[i]=='625' |
             foodData$com_sua_cod[i]=='626') {
    
    foodData$fdm_cod[i] <- 26
    foodData$com_cod[i] <- 3007
  }
  
  
  else if (foodData$com_sua_cod[i]=='656' | foodData$com_sua_cod[i]=='657'  | foodData$com_sua_cod[i]=='659') {
    
    foodData$fdm_cod[i] <- 44
    foodData$com_cod[i] <- 2630
  } 
  
  else if (foodData$com_sua_cod[i]=='667' | foodData$com_sua_cod[i]=='671'  | foodData$com_sua_cod[i]=='672') {
    
    foodData$fdm_cod[i] <- 45
    foodData$com_cod[i] <- 2635
  }
  
  else if (foodData$com_sua_cod[i]=='687' | foodData$com_sua_cod[i]=='689'  | foodData$com_sua_cod[i]=='698' |
             foodData$com_sua_cod[i]=='692' | foodData$com_sua_cod[i]=='693'  | foodData$com_sua_cod[i]=='702' |
             foodData$com_sua_cod[i]=='711' | foodData$com_sua_cod[i]=='720'  | foodData$com_sua_cod[i]=='723' ) {
    
    foodData$fdm_cod[i] <- 42
    foodData$com_cod[i] <- 2923
  }
  
  else if (foodData$com_sua_cod[i]=='564' | foodData$com_sua_cod[i]=='565') {
    
    foodData$fdm_cod[i] <- 47
    foodData$com_cod[i] <- 2655
  }
  
  else if (foodData$com_sua_cod[i]=='51') {
    
    foodData$fdm_cod[i] <- 48
    foodData$com_cod[i] <- 2656
  }  
  
  else if (foodData$com_sua_cod[i]=='26' | foodData$com_sua_cod[i]=='39'  | foodData$com_sua_cod[i]=='66' |
             foodData$com_sua_cod[i]=='82' | foodData$com_sua_cod[i]=='86'  | foodData$com_sua_cod[i]=='571' |
             foodData$com_sua_cod[i]=='634') {
    
    foodData$fdm_cod[i] <- 49
    foodData$com_cod[i] <- 3004
  }
  
  
  else if (foodData$com_sua_cod[i]=='109') {
    
    foodData$fdm_cod[i] <- 46
    foodData$com_cod[i] <- 2928
  }
  
  else if (foodData$com_sua_cod[i]=='867' | foodData$com_sua_cod[i]=='870'  | foodData$com_sua_cod[i]=='872' |
             foodData$com_sua_cod[i]=='873' | foodData$com_sua_cod[i]=='874'  | foodData$com_sua_cod[i]=='875' |
             foodData$com_sua_cod[i]=='876' | foodData$com_sua_cod[i]=='877'  | foodData$com_sua_cod[i]=='947' ) {
    
    foodData$fdm_cod[i] <- 27
    foodData$com_cod[i] <- 2731
  }
  
  else if (foodData$com_sua_cod[i]=='977' | foodData$com_sua_cod[i]=='1017' ) {
    
    foodData$fdm_cod[i] <- 28
    foodData$com_cod[i] <- 2732
  }
  
  else if (foodData$com_sua_cod[i]=='1035' | foodData$com_sua_cod[i]=='1038'  | foodData$com_sua_cod[i]=='1039' |
             foodData$com_sua_cod[i]=='1041' | foodData$com_sua_cod[i]=='1042') {
    
    foodData$fdm_cod[i] <- 29
    foodData$com_cod[i] <- 2733
  }
  
  else if (foodData$com_sua_cod[i]=='1058' | foodData$com_sua_cod[i]=='1060'  | foodData$com_sua_cod[i]=='1061' |
             foodData$com_sua_cod[i]=='1069' | foodData$com_sua_cod[i]=='1073' | foodData$com_sua_cod[i]=='1080') {
    
    foodData$fdm_cod[i] <- 30
    foodData$com_cod[i] <- 2734
  }
  
  
  else if (foodData$com_sua_cod[i]=='1089' | foodData$com_sua_cod[i]=='1097'  | foodData$com_sua_cod[i]=='1108' |
             foodData$com_sua_cod[i]=='1111' | foodData$com_sua_cod[i]=='1127'  | foodData$com_sua_cod[i]=='1141' |
             foodData$com_sua_cod[i]=='1151' | foodData$com_sua_cod[i]=='1158'  | foodData$com_sua_cod[i]=='1163' |
             foodData$com_sua_cod[i]=='1164' | foodData$com_sua_cod[i]=='1166'  | foodData$com_sua_cod[i]=='1172' |
             foodData$com_sua_cod[i]=='1176') {
    
    foodData$fdm_cod[i] <- 31
    foodData$com_cod[i] <- 2735
  }
  
  
  else if (foodData$com_sua_cod[i]=='868' | foodData$com_sua_cod[i]=='878'  | foodData$com_sua_cod[i]=='948' |
             foodData$com_sua_cod[i]=='978' | foodData$com_sua_cod[i]=='1018'  | foodData$com_sua_cod[i]=='1036' |
             foodData$com_sua_cod[i]=='1059' | foodData$com_sua_cod[i]=='1074'  | foodData$com_sua_cod[i]=='1075' |
             foodData$com_sua_cod[i]=='1081' | foodData$com_sua_cod[i]=='1098'  | foodData$com_sua_cod[i]=='1128' |
             foodData$com_sua_cod[i]=='1159' | foodData$com_sua_cod[i]=='1167') {
    
    foodData$fdm_cod[i] <- 32
    foodData$com_cod[i] <- 2736
  }
  
  else if (foodData$com_sua_cod[i]=='869' | foodData$com_sua_cod[i]=='871'  | foodData$com_sua_cod[i]=='949' |
             foodData$com_sua_cod[i]=='979' | foodData$com_sua_cod[i]=='994'  | foodData$com_sua_cod[i]=='1019' |
             foodData$com_sua_cod[i]=='1037' | foodData$com_sua_cod[i]=='1040'  | foodData$com_sua_cod[i]=='1043' |
             foodData$com_sua_cod[i]=='1065' | foodData$com_sua_cod[i]=='1066'  | foodData$com_sua_cod[i]=='1129' |
             foodData$com_sua_cod[i]=='1160' | foodData$com_sua_cod[i]=='1168' |
             foodData$com_sua_cod[i]=='1221' | foodData$com_sua_cod[i]=='1222'  | foodData$com_sua_cod[i]=='1225' |
             foodData$com_sua_cod[i]=='1243' ) {
    
    foodData$fdm_cod[i] <- 41
    foodData$com_cod[i] <- 2737
  }
  
  else if (foodData$com_sua_cod[i]=='886' | foodData$com_sua_cod[i]=='887'  | foodData$com_sua_cod[i]=='952' |
             foodData$com_sua_cod[i]=='953' | foodData$com_sua_cod[i]=='983'  | foodData$com_sua_cod[i]=='1022') {
    
    foodData$fdm_cod[i] <- 38
    foodData$com_cod[i] <- 2740
  }
  
  else if (foodData$com_sua_cod[i]=='885') {
    
    foodData$fdm_cod[i] <- 39
    foodData$com_cod[i] <- 2743
  }  
  
  else if (foodData$com_sua_cod[i]=='916' | foodData$com_sua_cod[i]=='1062'  | foodData$com_sua_cod[i]=='1063' |
             foodData$com_sua_cod[i]=='1064' | foodData$com_sua_cod[i]=='1091') {
    
    foodData$fdm_cod[i] <- 33
    foodData$com_cod[i] <- 2744
  }  
  
  else if (foodData$com_sua_cod[i]=='2745') {
    
    foodData$fdm_cod[i] <- 17
    foodData$com_cod[i] <- 2745
  }
  
  
  else if (foodData$com_sua_cod[i]=='1501' | foodData$com_sua_cod[i]=='1502'  | foodData$com_sua_cod[i]=='1503' |
             foodData$com_sua_cod[i]=='1504' | foodData$com_sua_cod[i]=='1505' |
             foodData$com_sua_cod[i]=='1506' | foodData$com_sua_cod[i]=='1507' |
             foodData$com_sua_cod[i]=='1508' |
             foodData$com_sua_cod[i]=='1514' | foodData$com_sua_cod[i]=='1515'  | foodData$com_sua_cod[i]=='1516' |
             foodData$com_sua_cod[i]=='1517' | foodData$com_sua_cod[i]=='1518' |
             foodData$com_sua_cod[i]=='1519' | foodData$com_sua_cod[i]=='1520' |
             foodData$com_sua_cod[i]=='1521' |
             foodData$com_sua_cod[i]=='1527' | foodData$com_sua_cod[i]=='1528'  | foodData$com_sua_cod[i]=='1529' |
             foodData$com_sua_cod[i]=='1530' | foodData$com_sua_cod[i]=='1531' |
             foodData$com_sua_cod[i]=='1532' | foodData$com_sua_cod[i]=='1533' |
             foodData$com_sua_cod[i]=='1534' |
             foodData$com_sua_cod[i]=='1540' | foodData$com_sua_cod[i]=='1541'  | foodData$com_sua_cod[i]=='1542' |
             foodData$com_sua_cod[i]=='1543' | foodData$com_sua_cod[i]=='1544' |
             foodData$com_sua_cod[i]=='1545' | foodData$com_sua_cod[i]=='1546' |
             foodData$com_sua_cod[i]=='1547') {
    
    foodData$fdm_cod[i] <- 34
    foodData$com_cod[i] <- 3001
  } 
  
  else if (foodData$com_sua_cod[i]=='1553' | foodData$com_sua_cod[i]=='1554'  | foodData$com_sua_cod[i]=='1555' |
             foodData$com_sua_cod[i]=='1556' | foodData$com_sua_cod[i]=='1557' |
             foodData$com_sua_cod[i]=='1558' |
             foodData$com_sua_cod[i]=='1570' | foodData$com_sua_cod[i]=='1571'  | foodData$com_sua_cod[i]=='1572' |
             foodData$com_sua_cod[i]=='1573' | foodData$com_sua_cod[i]=='1574' |
             foodData$com_sua_cod[i]=='1575' |
             foodData$com_sua_cod[i]=='1562' | foodData$com_sua_cod[i]=='1563'  | foodData$com_sua_cod[i]=='1564' |
             foodData$com_sua_cod[i]=='1565' | foodData$com_sua_cod[i]=='1566') {
    
    foodData$fdm_cod[i] <- 35
    foodData$com_cod[i] <- 3002
  } 
  
  else if (foodData$com_sua_cod[i]=='1580' | foodData$com_sua_cod[i]=='1583'  | foodData$com_sua_cod[i]=='1587' |
             foodData$com_sua_cod[i]=='1588' | foodData$com_sua_cod[i]=='1589' |
             foodData$com_sua_cod[i]=='1590' |
             foodData$com_sua_cod[i]=='1594' | foodData$com_sua_cod[i]=='1595'  | foodData$com_sua_cod[i]=='1596') {
    
    foodData$fdm_cod[i] <- 36
    foodData$com_cod[i] <- 3003
  } 
  
  else if (foodData$com_sua_cod[i]=='1509' | foodData$com_sua_cod[i]=='1522'  | foodData$com_sua_cod[i]=='1535' |
             foodData$com_sua_cod[i]=='1548' | foodData$com_sua_cod[i]=='1582' |
             foodData$com_sua_cod[i]=='1510' |
             foodData$com_sua_cod[i]=='1536' | foodData$com_sua_cod[i]=='1549') {
    
    foodData$fdm_cod[i] <- 41
    foodData$com_cod[i] <- 2737
  } 
  
  else if (foodData$com_sua_cod[i]=='27' | foodData$com_sua_cod[i]=='28'  | foodData$com_sua_cod[i]=='29' |
             foodData$com_sua_cod[i]=='31' | foodData$com_sua_cod[i]=='32' |
             foodData$com_sua_cod[i]=='33' |
             foodData$com_sua_cod[i]=='34' | foodData$com_sua_cod[i]=='35' | foodData$com_sua_cod[i]=='38') {
    
    foodData$fdm_cod[i] <- 2
    foodData$com_cod[i] <- 2805
  } 
  
  else if (foodData$com_sua_cod[i]=='882' | foodData$com_sua_cod[i]=='888'  | foodData$com_sua_cod[i]=='889' |
             foodData$com_sua_cod[i]=='890' | foodData$com_sua_cod[i]=='891' |
             foodData$com_sua_cod[i]=='892' | foodData$com_sua_cod[i]=='893' |
             foodData$com_sua_cod[i]=='894' | foodData$com_sua_cod[i]=='895'  | foodData$com_sua_cod[i]=='896' |
             foodData$com_sua_cod[i]=='897' | foodData$com_sua_cod[i]=='898' |
             foodData$com_sua_cod[i]=='899' | foodData$com_sua_cod[i]=='900' |
             foodData$com_sua_cod[i]=='901' | foodData$com_sua_cod[i]=='903'  | foodData$com_sua_cod[i]=='904' |
             foodData$com_sua_cod[i]=='907' | foodData$com_sua_cod[i]=='908' |
             foodData$com_sua_cod[i]=='909' | foodData$com_sua_cod[i]=='910' |
             foodData$com_sua_cod[i]=='917' | foodData$com_sua_cod[i]=='951'  | foodData$com_sua_cod[i]=='954' |
             foodData$com_sua_cod[i]=='955' | foodData$com_sua_cod[i]=='982' |
             foodData$com_sua_cod[i]=='984' | foodData$com_sua_cod[i]=='985' |
             foodData$com_sua_cod[i]=='1020' | foodData$com_sua_cod[i]=='1021'  | foodData$com_sua_cod[i]=='1023' |
             foodData$com_sua_cod[i]=='1130' | foodData$com_sua_cod[i]=='905') {
    
    foodData$fdm_cod[i] <- 37
    foodData$com_cod[i] <- 2848
  }
  
  else {
    foodData$fdm_cod[i] <- NA
    foodData$com_cod[i] <- NA
  }
  
}  

foodData <- foodData[!is.na(foodData$fdm_cod), ]


foodData$M49_cod <- faoswsUtil::fs2m49(as.character(foodData$country_cod))

GdpPopFoodDataM49 = merge(foodData, GdpPopData, by = "M49_cod", all.x = TRUE)

## set the keys to get the FDM data from the FAO working system (elasticities,
## functional form, etc.)
areaCodes = faosws::GetCodeList("food", "food_factors","geographicAreaM49")
comCodes = faosws::GetCodeList("food", "food_factors","foodCommodityM")
fdmCodes = faosws::GetCodeList("food", "food_factors","foodFdm")
funCodes = faosws::GetCodeList("food", "food_factors","foodFunction")

dim1 = Dimension(name = "geographicAreaM49", keys = areaCodes[,code]) 
dim2 = Dimension(name = "foodCommodityM", keys = comCodes[, code])       
dim3 = Dimension(name = "foodFdm", keys = fdmCodes[, code])
dim4 = Dimension(name = "foodFunction", keys = funCodes[, code] )
dim5 = Dimension(name = "foodVariable", keys = "y_e" )


key = DatasetKey(domain = "food", dataset = "food_factors",
                 dimensions = list(dim1, dim2, dim3, dim4, dim5))


## download the FDM data from the SWS
fdmData <- GetData(key, flags=FALSE, normalized = FALSE)

## change names of columns (variables) to something more logical
setnames(fdmData, c(names(fdmData)),
         c("M49_cod","com_cod","fdm_cod","func_form","elas"))

fdmData[,  M49_cod := as.numeric (M49_cod)]
fdmData[,  com_cod := as.numeric (com_cod)]
fdmData[,  fdm_cod := as.numeric (fdm_cod)]
fdmData[,  func_form := as.numeric (func_form)]
fdmData[,  elas := as.numeric (elas)]

data_base <- merge(GdpPopFoodDataM49, fdmData,  by.y = c("fdm_cod","M49_cod","com_cod"), 
                      by.x = c("fdm_cod","M49_cod","com_cod"), all.x = TRUE)

data_base$food_pc_2007 <- data_base$food_2007/data_base$pop_2007

data_base$food_pc_2008 <- data_base$food_2008/data_base$pop_2008

data_base$food_pc_2009 <- data_base$food_2009/data_base$pop_2009

data_base$food_pc_2010 <- data_base$food_2010/data_base$pop_2010

data_base$food_pc_2011 <- data_base$food_2011/data_base$pop_2011

## HACK: Remove rows with missing func_form.  This may not be the right thing to
## do: we may need to update the elasticities table.
data_base <- data_base[!is.na(data_base$func_form), ]
results <- FoodModule(data_base)
dataToSave <- # Geographic Area, measuredElement = 141, measuredItem = SUA item code, Dist Param = log(Mu) or log(Sigma), Year = Year

SaveData(domain = "suafbs", dataset = "fbs_distrib", data = )



## To do:
## - Implement SaveData function and make sure we can write to the FBS Distribution table
## - Check the input dataset from Josef against what's on the server: ask Nick for
## the file that Jim gave him with the elasticities, compare with Josef's file
## and figure out why we have differences.  Reload if necessary.
## - Come up with a way to get commodity trees out of the system (additional
## table? different hierarchy?)
