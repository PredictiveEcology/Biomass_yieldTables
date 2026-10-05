## What consumers of `yieldTablesCumulative` rely on, checked in one place.
##
## - Every table (yieldTableIndex x speciesCode) starts at age 0 with near-zero biomass.
##   LandRCBM_split3pools zeroes that row (`age == 0 & B <= 0.01` t/ha, i.e. <= 1 g/m2), takes
##   annual increments as c(0, diff(B)) from it, and CBM_core joins increments to cohorts by
##   (gcID, age), including age-0 cohorts after disturbance.
## - Ages then run 0, 1, 2, ... with no gaps or repeats, so row i is year i of growth.
## - Biomass is never negative, and no table goes past maxAge (a table ends when its cohort
##   dies, so tables can end earlier).
##
## Returns the rules that `yt` breaks (character(0) if none).
yieldTableViolations <- function(yt, maxAge = NULL) {
  yt <- data.table::as.data.table(yt)
  perTable <- yt[, list(minAge = min(age), maxAge = max(age), n = .N, nAges = data.table::uniqueN(age),
                        B0 = biomass[age == 0][1]), by = c("yieldTableIndex", "speciesCode")]
  as.character(c(if (!all(perTable$minAge == 0L)) "no age-0 row",
    if (any(!is.na(perTable$B0) & perTable$B0 > 1)) "age-0 biomass above 1 g/m2",
    if (!all(perTable$nAges == perTable$n)) "repeated ages",
    if (!all(perTable$n == perTable$maxAge - perTable$minAge + 1L)) "gaps in ages",
    if (any(yt$biomass < 0)) "negative biomass",
    if (!is.null(maxAge) && any(perTable$maxAge > maxAge)) "ages beyond maxAge"))
}

expectYieldTableContract <- function(yt, maxAge = NULL) {
  v <- yieldTableViolations(yt, maxAge)
  testthat::expect(length(v) == 0, paste("yield tables break the contract:", paste(v, collapse = "; ")))
  invisible(yt)
}
