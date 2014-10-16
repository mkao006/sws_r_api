# Computation Module for Imputationg of Missing Values

This is the module for the imputation of missing values in the
production domain.

### Depends

The module depends on the `faoswsUtil`, `faoswsFlag` and the
`faoswsProductionImputation` package which can be installed by running
the following command.

```r
library(devtools)
install_github(repo = "sws_flag", 
	       username = "mkao006", 
	       subdir = "faoswsFlag")

install_github(repo = "sws_util",
               username = "mkao006",
               subdir = "faoswsUtil")

install_github(repo = "sws_imputation", 
           username = "mkao006", 
           subdir = "faoswsProductionImputation")

```

In addition, the yield is assumed to be computed by the yield module a
priori.

