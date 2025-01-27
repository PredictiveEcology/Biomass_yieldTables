## SET UP ----


# to test all the test files in the tests folder:
testthat::test_dir(file.path("tests", "testthat"))

# Alternative, you can use test_file to test individual test file, e.g.:
testthat::test_file(file.path("tests", "testthat", "test-1-runBiomass_core.R"))
testthat::test_file(file.path("tests", "testthat", "test-2-ReadExperimentFiles.R"))
testthat::test_file(file.path("tests", "testthat", "test-3-generateYieldTables.R"))
testthat::test_file(file.path("tests", "testthat", "test-4-pltfn.R"))
testthat::test_file(file.path("tests", "testthat", "test-5-Biomass_yieldTables.R"))