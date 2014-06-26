## FAOSWS *flag* package

This is an add-on to the faosws package to support the manipulation
and the aggregation of flags.

## Aggregation

The aggregation of observational flags depends on the reliability of
the original sources. 

The following table shows the current observational flags and
information weights.

flagObservationStatus|flagObservationWeight
---------------------|---------------------
 |1
T|0.8
E|0.75
I|0.5
M|0
-------------------------------------------

The aggregation is done by taking the lowest weight to reflect the
reliability of the source.

## Example

### Computing Yield

Lets take the scenario of yield computation for example. If the source
of production was official ( ), while the area harvested was collected
from unofficial sources (T); then the observation status flag of yield
which is calculated by division of production by area harvested will
be (T).