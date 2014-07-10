# Computation Module for yield calculation

This is the module for computing the yield based on production and
area harvested.

### Depends

The module depends on the `faoswsExtra` and the `faoswsFlag` package
which can be installed by running the following command.

```r
library(devtools)
install_github(repo = "sws_r_api", 
	       username = "mkao006", 
	       subdir = "faoswsFlag/faoswsFlag")

install_github(repo = "sws_r_api",
               username = "mkao006",
               subdir = "faoswsExtra/faoswsExtra")
```


