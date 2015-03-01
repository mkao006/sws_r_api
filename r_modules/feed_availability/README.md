# Computation Module for Feed Availability

This is the module for computing the feed availability.


## Classification

* **Feed Only Commodities**

  Commodities such as "Cake of Rice Bran" which has no other use but
  for feed. 

* **Potential For Feed Items**

  This is a very general class where commodities which can be
  potentially used for feed such as wheat will be included in this
  category. 

* **Oil Seed** 

  For this category, we have to calculate the availability of the
  primary commodity then its potential production of oil and cakes
  based on OCBS crush rate and extraction rate.

* **Not For Feed Items**

  Commodities in this classification will not have any feed
  availability. This category includes commodity such as bread, which
  is used for consumption and none for feed.



The detailed list of the composition can be found under the `cpcFeedClassification.csv` file.

## The algorithm

Below we describe the algorithm assuming all other elements has been
calculated and merged into a single dataset.

1. **Compute Feed Availability based on the classification**

   * Feed Only Commodities

      * All production and trade are summed up and allocated to feed availability.

   * Potential For Feed Items

      * The feed availability is taken as the residual after supply
        has taken account of all utilization elements.

   * Oil Seeds
 
      * Similar to potential for feed items, we first take the
        residual, but then the commodities are then converted to oil
        and cakes based on the crush rate and oil/meal extaction rate.

   * Not for feed items

      * The feed availability will be zero for these items.

