##' Function to get the mapping between HS to CPC
##'
##' @return The mapping table between HS to CPC.

getHStoCPCMapping = function(){    
    mappingKey = MappingTableKey(mappingTable = "tradeHS2CPC")
    GetMapping(mappingKey)
}
