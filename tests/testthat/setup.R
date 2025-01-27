

if (!testthat::is_testing()){
  library(testthat)
  testthat::source_test_helpers(env = globalenv())
}

Require::Require("archive")

# Set teardown environment
teardownEnv <- if (testthat::is_testing()) testthat::teardown_env() else parent.frame()

# List test directories
testDirs <- .test_directories()

# Create temporary directories
for (d in testDirs$temp) dir.create(d)
withr::defer({
  unlink(testDirs$temp$root, recursive = TRUE)
  if (file.exists(testDirs$temp$root)) warning(
    "Temporary test directory could not be removed: ", testDirs$temp$root, call. = F)
}, envir = teardownEnv, priority = "last")

# Copy module to temporary location
## This will hopefully be handled by testthat if a DESCRIPTION file is added.
.test_copyModule(testDirs$Rproj, testDirs$temp$modules)

# Set reproducible options
withr::local_options(list(
  reproducible.verbose = -2,
  reproducible.cachePath = testDirs$temp$root # Example of caching location
), .local_envir = teardownEnv)

# Set SpaDES.project option to never update R profile
withr::local_options(list(SpaDES.project.updateRprofile = FALSE), .local_envir = teardownEnv)

# Source the function
source(file.path(testDirs$Rproj, "R", "pltfn.R"))
source(file.path(testDirs$Rproj, "R", "runBiomass_core.R"))
source(file.path(testDirs$Rproj, "R", "ReadExperimentFiles.R"))