library(faosws)
library(faoswsAupus)
library(faoswsUtil)
library(data.table)

## Connection detail to the new working system R API
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}

## Get item table
param = getAupusParameter(countryCode = "100", assignGlobal = FALSE)
getAupusDataset(param = param, assignGlobal = TRUE)

## Temporary solution for missing elements
missingColumns =
    c(paste0("Value_measuredElementFS_", c(541, 546)),
      paste0("flagFaostat_measuredElementFS_", c(541, 546)))
aupusData[, `:=`(c(missingColumns),
                 list(as.numeric(NA), as.numeric(NA), as.character(NA),
                      as.character(NA)))]



## Merge all the data
aupusFinal =
    mergeAll(aupusData = aupusData, itemInfoData = itemInfoData,
             balanceElementData = balanceElementData,
             shareData = shareData,
             inputData = inputData, param = param,
             inputNum = "Value_input",
             balanceElementNum = "balanceElement",
             shares = "Value_share",
             element131Num = "Value_measuredElementFS_131")

## Execute the Aupus module
aupusModule =
    try(
        {
            Aupus(aupusFinalData = aupusFinal, shareData = shareData,
                  itemTypeCol = "measuredItemTypeFS",
                  shareNum = "Value_share",
                  inputNum = "Value_input",
                  balanceElementNum = "balanceElement")

            updatedAupus =  aupusFinal[, colnames(aupusData),
                with = FALSE]
            setnames(updatedAupus, "timePointYearsSP", "timePointYears")
            
            SaveData(domain = "faostat_one", dataset = "FS1_SUA",
                     data = updatedAupus,
                     normalized = FALSE)
        })


if(inherits(aupusModule, "try-error")){
    print("Aupus Module Failed")
} else {
    print("Aupus Module Executed Successfully")
}



