

runBiomass_core <- function(moduleNameAndBranch, paths, cohortData, maxAge, species, simEnv, initialB,
                            headstartYears = 0L, headstartRegen = c("serotiny", "resprout")) {
  # get modules if using stand alone module
  if (!is.null(moduleNameAndBranch)) {
    getModule(moduleNameAndBranch, modulePath = paths$modulePath, overwrite = TRUE) # will only overwrite if wrong version
  }
  speciesNameConvention <- LandR::equivalentNameColumn(species$species, LandR::sppEquivalencies_CA)
  # initialize all cohorts as Biomass_core starts new cohorts (age 1, biomass of initialB);
  # the age-0 rows (biomass 0) are added after the simulation, by addAgeZeroRows()
  cohortDataForYield <- initYieldCohorts(cohortData, initialB)
  
  
  endTime <- ifelse(is.na(maxAge), 
                    max(species$longevity), 
                    min(maxAge, max(species$longevity)))
  # cohorts are age 1 at time 0, so time endTime - 1 holds age endTime
  timesForYield <- list(start = 0, end = endTime - 1)
  
  # The following line reduce the number of pixel groups. Pixels of the same 
  # ecoregion with the same species composition will produce the same Yield Tables. 
  # To optimise biomass_core we can reduce the number of pixelGroups accordingly,
  # and retrieve them later.
  
  # update pixelGroups
  newPixelGroups <- updatePixelGroups(cohortDataForYield) |> Cache()
  cohortDataForYield <- newPixelGroups$cohortData
  rcl <- as.matrix(
    cbind(is = newPixelGroups$pixelGroupRef$pixelGroup,
          becomes = newPixelGroups$pixelGroupRef$pixelGroupYield)
  )
  
  # pick out the elements of the simList that are relevant for Caching -- not everything is
  modPath <- file.path(paths$modulePath, "Biomass_core")
  filesToDigest <- c(dir(file.path(modPath, "R"), full.names = TRUE), file.path(modPath, "Biomass_core.R"))
  dig1 <- lapply(filesToDigest, function(fn) digest::digest(file = fn))
  dig <- CacheDigest(list(species, cohortDataForYield, timesForYield, dig1))
  simOutputs <- expand.grid(objectName = "cohortData",
                            saveTime = unique(seq(timesForYield$start, timesForYield$end, by = 1)),
                            eventPriority = 1,
                            fun = "qs2::qs_save",
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
  objects$pixelGroupMap <- terra::classify(objects$pixelGroupMap, rcl) |> Cache()
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
      , "minCohortBiomass" = 1
      , "initialB" = initialB
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
  
  headstart <- NULL
  if (!is.na(headstartYears) && headstartYears > 0) {
    headstart <- runHeadstart(cohortData = cohortDataForYield, objects = objects, parameters = parameters,
                              paths = paths, species = species, years = as.integer(headstartYears),
                              regen = headstartRegen, endTime = timesForYield$end, dig = dig)
  }

  list(simOutputs = simOutputs, digest = dig, yieldPixelGroupMap = objects$pixelGroupMap,
       yieldEcoregion = unique(cohortDataForYield[, .(yieldTableIndex = pixelGroup,
                                                      ecoregionGroup = as.character(ecoregionGroup))]),
       headstart = headstart)
}

## Yield tables in which the species that regenerate after fire from seed banks or resprouts
## (`species$postfireregen` in `regen`) grow alone for `years` before the others are added (age 1, `initialB`,
## as in the plain tables). Only pixelGroups with a head-start species and at least two species can differ from
## the plain tables, so only those are run. Returns the outputs of both phases (phase 1: times 0 to `years`,
## phase 2: `years` to `end`) and the pixelGroups that were run.
headstartGroups <- function(cohortData, headstartSpecies) {
  perGroup <- cohortData[, .(nSpecies = uniqueN(speciesCode),
                             anyHeadstart = any(as.character(speciesCode) %in% headstartSpecies)),
                         by = pixelGroup]
  perGroup[nSpecies > 1L & anyHeadstart, pixelGroup]
}

runHeadstart <- function(cohortData, objects, parameters, paths, species, years, regen, endTime, dig) {
  headstartSpecies <- species$species[species$postfireregen %in% regen]
  isHeadstart <- as.character(cohortData$speciesCode) %in% headstartSpecies
  groups <- headstartGroups(cohortData, headstartSpecies)
  if (length(groups) == 0L) {
    message("Headstart: no multi-species pixelGroup has a species that regenerates by ",
            paste(regen, collapse = " or "), "; no headstart tables")
    return(NULL)
  }
  if (years >= endTime) {
    stop("headstartYears (", years, ") must be shorter than the simulated years (", endTime, ")")
  }
  message("Headstart: ", paste(headstartSpecies, collapse = ", "), " grow alone for ", years, " years in ",
          length(groups), " of ", uniqueN(cohortData$pixelGroup), " pixelGroups")

  ## pixelGroups that are not run are set to 0 (no vegetation) in the map
  groupMap <- terra::deepcopy(objects$pixelGroupMap)
  v <- terra::values(groupMap)[, 1]
  v[!is.na(v) & !v %in% groups] <- 0L
  terra::values(groupMap) <- v

  runPhase <- function(label, cd, times, extraDigest) {
    phaseObjects <- objects
    phaseObjects$cohortData <- cd
    phaseObjects$pixelGroupMap <- groupMap
    phasePaths <- paths
    # illustrative only: written to the session's tempdir (removed by `headstartTables`' caller), never cached
    phasePaths$outputPath <- file.path(tempdir(), paste0("cohortDataYieldHeadstart", label))
    phaseOutputs <- expand.grid(objectName = "cohortData", saveTime = seq(times$start, times$end, by = 1),
                                eventPriority = 1, fun = "qs2::qs_save", stringsAsFactors = FALSE)
    phaseParams <- parameters
    phaseParams$Biomass_core$.plotInitialTime <- times$start
    phaseDigest <- CacheDigest(list(dig, "headstart", label, times, headstartSpecies, extraDigest))
    out <- simInitAndSpadesClearEnv(paths = phasePaths, times = times, params = phaseParams,
                                    modules = "Biomass_core", outputs = phaseOutputs, objects = phaseObjects,
                                    debug = 1)
    list(simOutputs = out, digest = phaseDigest)
  }

  phase1 <- runPhase("1", cohortData[isHeadstart], list(start = 0, end = years), NULL)
  handoffFile <- as.data.table(phase1$simOutputs)[saveTime == years & saved == TRUE, file]
  headstartCohorts <- qs2::qs_read(handoffFile)
  lateCohorts <- cohortData[pixelGroup %in% groups & !isHeadstart]
  cohortsAtHandoff <- rbindlist(list(headstartCohorts, lateCohorts), use.names = TRUE, fill = TRUE)
  phase2 <- runPhase("2", cohortsAtHandoff, list(start = years, end = endTime), cohortsAtHandoff)

  list(phase1 = phase1$simOutputs, phase2 = phase2$simOutputs, years = years, groups = groups,
       digest = list(phase1$digest, phase2$digest))
}

#' Start cohorts as Biomass_core starts new cohorts: age 1 and biomass `initialB`.
#' @param cohortData a `cohortData` table.
#' @param initialB numeric, the `initialB` parameter passed on to Biomass_core.
#' @return a copy of `cohortData` with `B` and `age` reset.
initYieldCohorts <- function(cohortData, initialB) {
  cohortDataForYield <- copy(cohortData)
  cohortDataForYield$B <- as.integer(round(initialB))
  cohortDataForYield$age <- 1L
  cohortDataForYield
}

#' Add an age-0 row with biomass 0 to every yield table.
#'
#' The simulated tables start at age 1 (biomass `initialB`). Consumers need an age-0 row:
#' LandRCBM_split3pools takes increments from it, and CBM_core joins increments to age-0 cohorts.
#' @param yieldTables a `data.table` with columns `yieldTableIndex`, `speciesCode`, `age`, `biomass`.
#' @return `yieldTables` with one age-0 row (biomass 0) per table, keyed as before.
addAgeZeroRows <- function(yieldTables) {
  age0 <- yieldTables[, list(age = 0L, biomass = 0L), by = c("yieldTableIndex", "speciesCode")]
  out <- rbindlist(list(age0, yieldTables), use.names = TRUE, fill = TRUE)
  setorderv(out, c("speciesCode", "yieldTableIndex", "age"))
  setkeyv(out, c("speciesCode", "yieldTableIndex"))
  out
}

simInitAndSpadesClearEnv <- function(...) {
  simOut <- simInitAndSpades(...)
  rm(list = ls(simOut, all.names = TRUE), envir = envir(simOut))
  outputs(simOut)
}