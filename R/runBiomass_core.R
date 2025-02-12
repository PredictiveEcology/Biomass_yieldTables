

runBiomass_core <- function(moduleNameAndBranch, paths, cohortData, species, simEnv) {
  # get modules if using stand alone module
  if (!is.null(moduleNameAndBranch)) {
    getModule(moduleNameAndBranch, modulePath = paths$modulePath, overwrite = TRUE) # will only overwrite if wrong version
  }
  speciesNameConvention <- LandR::equivalentNameColumn(species$species, LandR::sppEquivalencies_CA)
  # initialize all cohorts to age 0, biomass of 1, and simulation time to largest longevity
  cohortDataForYield <- copy(cohortData)
  cohortDataForYield$B <- 1L
  cohortDataForYield$age <- 0L
  timesForYield <- list(start = 0, end = max(species$longevity))
  
  # The following line reduce the number of pixel groups. Pixels of the same 
  # ecoregion with the same species composition will produce the same Yield Tables. 
  # To optimise biomass_core we can reduce the number of pixelGroups accordingly,
  # and retrieve them later.
  
  # update pixelGroups
  newPixelGroups <- updatePixelGroups(cohortDataForYield)
  cohortDataForYield <- newPixelGroups$cohortData
  rcl <- as.matrix(
    cbind(is = newPixelGroups$pixelGroupRef$oldPixelGroup,
          becomes = newPixelGroups$pixelGroupRef$newPixelGroup)
  )
  
  # pick out the elements of the simList that are relevant for Caching -- not everything is
  modPath <- file.path(paths$modulePath, "Biomass_core")
  filesToDigest <- c(dir(file.path(modPath, "R"), full.names = TRUE), file.path(modPath, "Biomass_core.R"))
  dig1 <- lapply(filesToDigest, function(fn) digest::digest(file = fn))
  
  dig <- CacheDigest(list(species, cohortDataForYield, timesForYield, dig1))
  simOutputs <- expand.grid(objectName = "cohortData",
                            saveTime = unique(seq(timesForYield$start, timesForYield$end, by = 1)),
                            eventPriority = 1,
                            fun = "qs::qsave",
                            # fun = "assign", # arguments = list(value = ll),
                            stringsAsFactors = FALSE)
  modulePath <- paste0(paths$modulePath)
  io <- inputObjects(module = "Biomass_core", path = modulePath)
  objectNames <- io$Biomass_core$objectName
  objectNames <- objectNames[sapply(objectNames, exists, envir = simEnv)]
  objects <- mget(objectNames, envir = simEnv)
  objects$cohortData <- cohortDataForYield
  objects$speciesEcoregion$year <- timesForYield$start
  message("Reclassifying pixelGroups, may take a few minutes...")
  objects$pixelGroupMap <- terra::classify(objects$pixelGroupMap, rcl)
  opts <- options("LandR.assertions" = FALSE)
  on.exit(options(opts))
  parameters <- list(
    .globals = list(
      "sppEquivCol" = speciesNameConvention),
    Biomass_core = list(
      ".plotInitialTime" = timesForYield$start
      , ".plots" = NULL#c("screen", "png")
      , ".saveInitialTime" = NULL#times$start
      , ".useCache" = c(".inputObjects", "init")
      , ".useParallel" = 1
      , ".maxMemory" = 30
      , "vegLeadingProportion" = 0
      , "calcSummaryBGM" = NULL
      , "seedingAlgorithm" = "noSeeding"
    )
  )
  paths$outputPath <- file.path(paths$outputPath, "cohortDataYield")
  # run Biomass_core without dispersal
  simOutputs <- Cache(simInitAndSpadesClearEnv,
                      paths = paths,
                      times = timesForYield,
                      params = parameters,
                      modules = "Biomass_core",
                      outputs = simOutputs,
                      objects = objects,
                      debug = 1, omitArgs = c("objects", "times", "debug"), .cacheExtra = dig
  )
  
  list(simOutputs = simOutputs, digest = dig, pixelGroupRef = newPixelGroups$pixelGroupRef)
}

simInitAndSpadesClearEnv <- function(...) {
  simOut <- simInitAndSpades(...)
  rm(list = ls(simOut, all.names = TRUE), envir = envir(simOut))
  outputs(simOut)
}