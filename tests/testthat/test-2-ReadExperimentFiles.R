test_that("function ReadExperiment works", {
  
  factorialOutputs <- read.csv(file.path(test_path(), "testdata", "factorialOutputs.csv"))
  factorialOutputs$file <- file.path(test_path(), factorialOutputs$file)
  out <- ReadExperimentFiles(factorialOutputs)
  
  expect_is(out, "data.table")
  expect_true(all(names(out) == c("speciesCode", "age", "B", "pixelGroup")))
  
})
