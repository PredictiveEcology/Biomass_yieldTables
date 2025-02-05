test_that("function ReadExperimentFiles works", {
  packages = c("data.table", "SpaDES.core")
  init.test.packages(packages)
  # This test runs the function ReadExperimentFiles
  # It uses the outputs produced by the previous test: test-1-runBiomass_core.R
  
  factorialOutputs <- read.csv(file.path(test_path(), "fixtures", "factorialOutputs.csv"))
  out <- .SpaDESwithCallingHandlers(ReadExperimentFiles(factorialOutputs))
  
  expect_is(out, "data.table")
  expect_true(all(names(out) == c("speciesCode", "age", "B", "pixelGroup")))
  
  unload.test.packages(packages)
  
})
