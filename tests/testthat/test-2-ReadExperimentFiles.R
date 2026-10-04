test_that("function ReadExperimentFiles works", {
  # This test runs the function ReadExperimentFiles
  # It uses the outputs produced by the previous test: test-1-runBiomass_core.R
  factorialOutputs <- read.csv(test_path("testdata", "factorialOutputs.csv"))
  out <- suppressMessages(ReadExperimentFiles(factorialOutputs))
  
  expect_is(out, "data.table")
  expect_named(out, c("speciesCode", "age", "biomass", "yieldTableIndex"))
  expect_equal(max(out$age), nrow(factorialOutputs)-1)
  
})
