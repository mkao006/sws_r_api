# Computation Module for Yield Calculation

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


### Definition of yield

The definition of yield is defined by the following equation:

``` mathjax
  \text{P}_t := \text{A}_t \times \text{Y}_t \quad\quad P_t \ge 0,\, A_t
  \ge 0,\, Y_t > 0
```

Unlike previous implementation, yield is a missing value if both and
area harvested and production are zero. First of all, division by zero
is NOT zero. Secondly, when both production and area harvested are
zero it does not imply that that the yield is zero. It is simply
unobserved through our indirect calculation.