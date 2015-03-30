##' Get All Countries
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

getAllCountries = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "geographicAreaM49")[type == "country", code]
}