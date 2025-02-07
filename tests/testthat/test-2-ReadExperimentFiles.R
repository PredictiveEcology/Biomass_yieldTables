test_that("function ReadExperimentFiles works", {
  # This test runs the function ReadExperimentFiles
  # It uses the outputs produced by the previous test: test-1-runBiomass_core.R
  factorialOutputs <- read.csv(file.path(spadesTestPaths$testdata, "factorialOutputs.csv"))
  out <- SpaDEStestMuffleOutput(ReadExperimentFiles(factorialOutputs))
  
  expect_is(out, "data.table")
  expect_true(all(names(out) == c("speciesCode", "age", "B", "pixelGroup")))
  
})
