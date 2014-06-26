# FAOSWS Flag package

This is an add-on to the faosws package to support the manipulation
and the aggregation of flags.

## Installation

The package can be installed by running the following command.

```r
library(devtools)
install_github(repo = "sws_r_api", username = "mkao006", 
               subdir = "faoswsFlag/faoswsFlag")
```


## Aggregation

The aggregation of observational flags depends on the reliability of
the original sources. 

The following table shows the current observational flags and
information weights.

-------------------------------------------------------
flagObservationStatus|flagObservationWeight|description
---------------------|---------------------|-----------
\<blank\> |1|Official figure
T|0.8|Unofficial figure
E|0.75|Estimate
I|0.5|Imputed
M|0|Missing
-------------------------------------------------------

The aggregation is done by taking the lowest weight to reflect the
reliability of the source.

## Example

### Computing Yield

Lets take the scenario of yield computation for example. If the source
of production was official ( ), while the area harvested was collected
from unofficial sources (T); then the observation status flag of yield
which is calculated by division of production by area harvested will
be (T).