## Get trade data










    

## NOTE (Michael): In order to build the trade profile, we need
##                 to know how to calculate the "trade". Do we
##                 include processed?  If no, then need the set of
##                 primary commodities, and meat for livestock; if
##                 yes, then we should only mapped to all the
##                 commodities which are within the standardization
##                 path.




## Pseudo codes:
##

## For primary:
## ---------------------------------------------------------------------
## (1) Obtain energy, protein, lipid, CHOLAVLDF and Sugar from meHS
## and sdHS. What is the difference between the two dataset. Also,
## what is the difference between the nutrients data used for trade
## and production. 

## (2) Obtain the edible, extraction rate and commodity from file from
## HSm.

## (3) Merge the consolidated trade data with the nutrient from step (1) and (2).

## (4) Calculate the energy, protein, fat, and CHOAVLDF for the trade.

## (5) Calculate the residual for feed, it is defined the same way as
## production. trade multiplied by (1 - extraction rate) then the
## nutrient conversion ratio. Probably should move this to domestic
## supply.

## (6) Aggregate the trade (both import and export), and also the
## columns calculated in step (5) and (6) by area, year, group and
## commodity. Again, need to understand the difference between the use
## of group and commodity.

## Domestic supply:
## (7) Add production and trade energy to obtain the domestic energy supply.

## For Processed:
## ---------------------------------------------------------------------

## (1) Load the D2D3s data set, what is the source of this data?





## Run production compiler to obtain the energy supply of each
## commodity group.


## Get all the HS code using GetCodeTree, then merge with Adam's
## file. Any HS code not mapped will go into HS to commodity group.

## Merge the energy supply by commodity with Adam's mapping, then
## determine the relative ratio. This ratio does not imply that the
## quantity factually came from these group and require sufficient
## availability, rather they suggests how the processed product was
## constructed.

## However, it is possible that the standardized export can be greater
## than the domestic supply. This needs to be checked.

## Build the relative production profile ratio for each parent
## commodity group for each country and year.

## Merge the import of the mirrored trade flow data and the exporter
## production profile, then standardize to obtain the standardized
## import.

## Merge the export of the mirrored trade flow data and the domestic
## production profile, then standardize to obtain the standardized
## export.


## All I need at the end is the trade in commodity group in per capita
