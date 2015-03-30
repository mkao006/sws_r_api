##' Get All Years
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

getAllYears = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "timePointYears")[description != "wildcard", code]
}