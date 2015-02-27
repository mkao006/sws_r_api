# Computation Module for Trade Mirroring

This is the module for mirroring missing reporting values via partner
countries.


## The algorithm

1. **Removing self-trade**

   In this step, we remove trade where the reporting country is equivalent to partner country.

2. **Remove inconsistent quantity and value**

   Inconsistency occurs when either one of quantity is non-zero while
   the respective is zero. In this case, we replace the zero with
   missing value and estimate at a later date.

3. **Add re-trade to trade**

   We simplify the calculation by consider re-import as import and
   re-export as export, and add them to their respective counterpart.

4. **Perform mirroring**

   Finally, we perform the mirroring process. This is carried out by
   filling in all trade reported, but were not recorded by the
   reciprocal partner.

5. **Calculate Unit Value**

   The unit value is then calculated.

