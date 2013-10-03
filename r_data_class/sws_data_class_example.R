########################################################################
## Title: Example for R sws data class
## Date: 2013-09-16
########################################################################

## Define parameter class
setClass(Class = "sws_data_param",
         representation(area = "character",
                        domain = "character",
                        item = "character",
                        element = "character"),
         prototype(area = "*",
                   domain = "*",
                   item = "*",
                   element = "*")
         )

## Define data class
setClass(Class = "sws_data_class",
         representation(data = "data.table",
                        key = "sws_data_param",
                        tag = "character",
                        timestamp = "character"),
         prototype(data = data.table(),
                   key = new("sws_data_param"),
                   tag = "mk",
                   timestamp = as.character(Sys.time()))
         )


## Create the class object
sws_data.sws = new("sws_data_class")



## Validation
##
## NOTE (Michael): data can not be larger than 10000 rows.
setValidity("sws_data_class",
            function(object){
              retrivel = NULL
              if(NROW(object@data) >= 10000)
                retrivel = "data oject too large"
              if(is.null(retrivel))
                return(TRUE)
              else
                return(retrivel)
            }
            )

## Length of domain, item and element codes must be the same.
setValidity("sws_data_param",
            function(object){
              retrivel = NULL
              slotLengths = length(unique(c(
                length(slot(object = object, name = "domain")),
                length(slot(object = object, name = "item")),
                length(slot(object = object, name = "element"))
                )))
              if(slotLengths > 1)
                retrivel = "Length of domain, item and element codes are not all the same"
              if(is.null(retrivel))
                return(TRUE)
              else
                return(retrivel)
            }            
            )

## creating an object with a data more than 10000 row will result in
## an error.
big_data.sws = new("sws_data_class",
  data = data.table(a = rnorm(100000), b = runif(100000)))

diff_param_length = new("sws_data_param",
  domain = c("1111", "2222"))


