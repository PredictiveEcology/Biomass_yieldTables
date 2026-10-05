# Biomass_yieldTables (development version)

* Reverted the start of yield-table cohorts at age 1 and `initialB` (#24): tables again start at age 0 with B = 1, which LandRCBM_split3pools and CBM_core rely on (an age-0 row per table, increments from age 0). New tests state what consumers rely on: every table starts at age 0 with biomass <= 1 g/m2, and ages run 0, 1, 2, ... without gaps or repeats.
* `reqdPkgs` now lists `crayon`, `digest`, `ggplot2`, `reproducible` and `SpaDES.project`, which the module's code uses.
