# Computation Module for aggregation

This module computes the aggregation either by geographic area or by
CPC commodity items.

### Depends

The module depends on the `faoswsUtil` package which can be installed
by running the following command.

```r
library(devtools)
install_github(repo = "sws_util", 
	       username = "mkao006", 
	       subdir = "faoswsUtil")
```

### Note

The current implementation requires the user to know the code which
corresponds to the parent node of the aggregation. For example, to
aggregate by all economic areas the parameter `aggregationCode` will
have to be set to 1061. A dynamic list will be required from the
developers to enable the user to choose which aggregation desired.

The graph for the geographic codes only has one layer, while the CPC
item has multiple layers.

### Check

There are duplicated items from the result of `GetCodeTree`.