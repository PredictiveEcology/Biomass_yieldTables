# Biomass_yieldTables (development version)

* Yield tables now start cohorts at age 1 and biomass `initialB` (new parameter, default 10, passed to Biomass_core), as Biomass_core does for new cohorts, instead of age 0 and biomass 1. Starting at 1 g/m2 let integer rounding decide which species got ahead in mixed stands. The age-0 relabelling in `GenerateYieldTables` is removed because ages now count up from 1.
* `reqdPkgs` now lists `crayon`, `digest`, `ggplot2`, `reproducible` and `SpaDES.project`, which the module's code uses.
