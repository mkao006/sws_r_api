#' Removes records where reporter and partner are the same
#' 
removeSelfTrade = function(data, reportingCountry, partnerCountry){
    data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
}
