
# Get a list of test directory paths
## These will need to be updated if a DESCRIPTION file is added.
.test_directories <- function(tempDir = tempdir()){
  
  testDirs <- list()
  
  # Set R project location
  testDirs$Rproj <- ifelse(testthat::is_testing(), dirname(dirname(getwd())), getwd())
  
  # Set input data path (must be absolute)
  testDirs$testdata <- file.path(testDirs$Rproj, "tests/testthat", "testdata")
  
  # Set temporary directory paths
  testDirs$temp <- list(
    root = file.path(tempDir, paste0("testthat-", basename(testDirs$Rproj)))
  )
  testDirs$temp$modules  <- file.path(testDirs$temp$root, "modules")  # For modules
  testDirs$temp$inputs   <- file.path(testDirs$temp$root, "inputs")   # For shared inputs
  testDirs$temp$libPath  <- file.path(testDirs$temp$root, "library")  # R package library
  testDirs$temp$outputs  <- file.path(testDirs$temp$root, "outputs")  # For unit test outputs
  testDirs$temp$projects <- file.path(testDirs$temp$root, "projects") # For project directories
  
  # Return
  testDirs
}

# Helper function: copy module
## This will hopefully be handled by testthat if a DESCRIPTION file is added.
.test_copyModule <- function(moduleDir, destDir, moduleName = basename(moduleDir)){
  
  modFiles <- file.info(list.files(moduleDir, full = TRUE))
  modFiles$name <- basename(row.names(modFiles))
  modFiles$path <- row.names(modFiles)
  
  modDir <- file.path(destDir, moduleName)
  dir.create(modDir)
  
  file.copy(modFiles$path[!modFiles$isdir],
            file.path(modDir, modFiles$name[!modFiles$isdir]))
  for (d in modFiles$path[modFiles$isdir & modFiles$name %in% c("R", "data")]){
    file.copy(d, modDir, recursive = TRUE)
  }
}

# Helper function: suppress output and messages; muffle common warnings
.SpaDESwithCallingHandlers <- function(expr, ...){
  
  if (testthat::is_testing()){
    
    withr::local_output_sink(tempfile())
    
    withCallingHandlers(
      expr,
      message = function(c) tryInvokeRestart("muffleMessage"),
      packageStartupMessage = function(c) tryInvokeRestart("muffleMessage"),
      warning = function(w){
        if (getOption("spadesCBM.test.suppressWarnings", default = FALSE)){
          tryInvokeRestart("muffleWarning")
        }else{
          if (grepl("^package ['\u2018]{1}[a-zA-Z0-9.]+['\u2019]{1} was built under R version [0-9.]+$", w$message)){
            tryInvokeRestart("muffleWarning")
          }
        }
      },
      ...
    )
    
  }else expr
}

# Handling package
# Initialize packages: 
# Save already loaded packages, install and load required package
init.test.packages <- function(pkg){
  repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
  if (!requireNamespace("Require", quietly = TRUE)) {
    install.packages("Require")
  }
  
  # Load Require
  library(Require)
  
  # Use Require to install and load packages
  Require(packages, dependencies = TRUE, repos = repos)
  
}

unload.test.packages <- function(loaded_pkgs){
  # Unload them
  for (pkg in loaded_pkgs) {
    try(detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE), silent = TRUE)
  }
}
