## Building the package
## ---------------------------------------------------------------------

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsFlag"))
    unlink("faoswsFlag", recursive = TRUE)

## Build the package
package.skeleton("faoswsFlag", code_files = paste("./codes/",
                           dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## build the flag table data
dir.create("faoswsFlag/data")
save(read.csv(file = "flag_table.csv", header = TRUE),
     file = "data/flag_table.Rdata")

## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsFlag/",
          overwrite = TRUE)
unlink("./faoswsFlag/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsFlag")
unlink("./faoswsFlag/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsFlag")
system("R CMD build faoswsFlag")
system("R CMD check --as-cran faoswsFlag")

