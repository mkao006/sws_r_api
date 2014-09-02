## Building the package
## ---------------------------------------------------------------------

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsExtra"))
    unlink("faoswsExtra", recursive = TRUE)

## Build the package
package.skeleton("faoswsExtra", code_files = paste("./codes/",
                           dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)


## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsExtra/",
          overwrite = TRUE)
unlink("./faoswsExtra/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsExtra")
unlink("./faoswsExtra/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsExtra")
system("R CMD build faoswsExtra")
## system("R CMD check --as-cran faoswsExtra")

