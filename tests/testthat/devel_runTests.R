

## SET UP ----

# Get needed packages
packages = c("SpaDES.core", "SpaDES.project", "LandR", "data.table", "digest", "reproducible", "terra", "ggplot2")
if (!requireNamespace("Require", quietly = TRUE)) {
  install.packages("Require")
}
Require::Require(packages, dependencies = TRUE, repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))))

# Suppress warnings from calls to setupProject, simInit, and spades
options("spades.test.suppressWarnings" = TRUE)

# Set custom input data location
options("reproducible.inputPaths" = NULL)

## RUN ALL TESTS ----
# Run all tests
testthat::test_dir(file.path("tests", "testthat"))

# Run all tests with different reporters
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::LocationReporter)
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::SummaryReporter)

## RUN INDIVIDUAL TESTS ----
testthat::test_file(file.path("tests", "testthat", "test-1-runBiomass_core.R"))
testthat::test_file(file.path("tests", "testthat", "test-2-ReadExperimentFiles.R"))
testthat::test_file(file.path("tests", "testthat", "test-3-generateYieldTables.R"))
testthat::test_file(file.path("tests", "testthat", "test-4-pltfn.R"))
testthat::test_file(file.path("tests", "testthat", "test-5-updatePixelGroups.R"))
testthat::test_file(file.path("tests", "testthat", "test-6-Biomass_yieldTables.R"))
