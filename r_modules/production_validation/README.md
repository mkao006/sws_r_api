## Validation Module for the Production Domain

This is the module which validates the value of production based on
the history.

### Method

The module extract all possible values which were ever inputed in the
database, but transform the values in to binary values depending on
whether the data was over written. Current cell value are marked as
valid, while historical values which has been over-written are
flagged as invalid.

The transformed binary variable is then fed into a set of
classification algorithms which will learn the classification
boundaries which maximize separation between valid and invalid values
for future prediction.

Current the module consists of five models, a committee voting
approach is taken. The prediction of the models are summed up to
produce a final `Severity` level. For example, if only one model
classified the value as invalid, then the value will have a severity
level of 1 and deserves some attention; on the other hand, if all 5
models flagged the value, then it will have a severity level of 5 and
requires immediate attention and revision.

## Models

Currently, the following 5 models are implemented.

1. Boosted Logistic Regression
2. Least Squares Support Vector Machine with Radial Basis Function Kernel
3. Simple Random Forest
4. Flexible Discriminant Analysis
5. Bagged Multivariate Adaptive Regression Splines.


**Warning: The algorithm is very computational heavy, and should not
   be executed unless on a very small dataset**