## The Biomass_borealDataPrep output that the module-level tests start from. It is
## a plain saveSimList() .rds, so it loads the same way on every platform; see
## test-1-runBiomass_core.R for how the simulation was made.
loadSmallSim <- function() {
  suppressWarnings(
    SpaDES.core::loadSimList(testthat::test_path("testdata", "smallSimOut.rds"),
                             projectPath = tempfile("smallSimOut"))
  )
}

