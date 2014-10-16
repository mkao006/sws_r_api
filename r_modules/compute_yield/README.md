# Computation Module for Yield Calculation

This is the module for computing the yield based on production and
area harvested.

### Depends

The module depends on the `faoswsUtil` and the `faoswsFlag` package
which can be installed by running the following command.

```r
library(devtools)
install_github(repo = "sws_flag", 
	       username = "mkao006", 
	       subdir = "faoswsFlag")

install_github(repo = "sws_util",
               username = "mkao006",
               subdir = "faoswsUtil")
```


### Definition of yield

The yield is defined as the ratio between production and area
harvested. The production is divided by the area harvested to obtain
the yield.

Unlike previous implementation, yield is a missing value if both and
area harvested and production are zero. First of all, division by zero
is NOT zero. Secondly, when both production and area harvested are
zero it does not imply that that the yield is zero. It is simply
unobserved through our indirect calculation.