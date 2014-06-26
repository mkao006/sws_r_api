##' Function to convert string to lower camel case
##'
##' @param string The character string
##'
##' @export

toLowerCamel = function(string){
    splitStrings = strsplit(string, split = "[[:punct:]]|[[:space:]]")
    lowerSplitStrings =
        lapply(splitStrings,
               FUN = function(x){
                   if(length(x) == 1) x
                   else tolower(x)
               })
    capwords = function(x){
        paste0(toupper(substring(text = x, first = 1, last = 1)),
               substring(text = x, first = 2))
    }
    lowerCamel = sapply(lowerSplitStrings,
        FUN = function(x) paste0(c(tolower(x[1]), capwords(x[-1])),
            collapse = ""))
    lowerCamel
}
