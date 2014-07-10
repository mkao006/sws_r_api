# Computation Module for aggregation

This module computes the aggregation either by geographic area or by
CPC commodity items.

### Depends

The module depends on the `faoswsExtra` package which can be installed
by running the following command.

```r
library(devtools)
install_github(repo = "sws_r_api", 
	       username = "mkao006", 
	       subdir = "faoswsFlag/faoswsFlag")
```

### Note

The current implementation requires the user to know the code which
corresponds to the parent node of the aggregation. For example, to
aggregate by all economic areas the parameter `aggregationCode` will
have to be set to 1061.