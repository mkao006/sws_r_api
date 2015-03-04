# Computation Module for Consolidate Trade

This is the module for aggregating all trade flow into consolidated
trade.


## The algorithm

1. **Consolidate Trade**

   Trading with different partner are aggregated into a single entry.

2. **Map UNSD Comtrade country code to Standard M49 country code**

   Comtrade employs a modified M49 country standard, here we convert
   this into the standard M49 used by all other FAO Statistical
   Working System domain.

