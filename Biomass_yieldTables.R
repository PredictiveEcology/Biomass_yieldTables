## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "Biomass_yieldTables",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Eliot"), family = "McIntire", role = c("aut", "cre"),
                                email = "eliot.mcintire@nrcan-rncan.gc.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(Biomass_yieldTables = "0.0.8"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_yieldTables.Rmd")), ## same file
  reqdPkgs = list("data.table", "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)"),
  parameters = rbind(
    defineParameter(".useCache", "character", c("generateData", "generateYieldTables"), NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 40, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted"),
    defineParameter("moduleNameAndBranch", "character", "Biomass_core@development (>= 1.3.9)", NA, NA,
                    "The branch and version number required for Biomass_core. This will be downloaded ",
                    "into the file.path(dataPath(sim), 'module') of this module, so it does not ",
                    "interact with the main user's modules. If this is set to NULL, then ",
                    "this module will not download a new copy of Biomass_core, but will use ",
                    "the existing one in the modulePath(sim)"),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If NA, a hash of studyArea will be used.")
  ),
  inputObjects = bindrows(
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "simOutputs", objectClass = "data.frame",
                  desc = "A data.frame showing the cohortData files that were created during this ",
                  "module. Normally, these can be deleted as they are re-read within this module ",
                  "and converted into CBM_AGB and CBM_speciesCodes."),
    createsOutput(objectName = "CBM_AGB", objectClass = "data.table",
                  "A large object intended to supply the eventual requirements for a CBM growth increment object. ",
                  "This one will have column names of pixelGroup, age, Sp1, Sp2, Sp3. The last three ",
                  "columns represent aboveground biomass of that species at that age. Note, these ",
                  "will not be strictly increasing as the species approaches its longevity. Sp1 ",
                  "will always be the species in the pixelGroup with the highest maximum biomass; Sp2 is ",
                  "second highest; Sp3 is 3rd highest. To see what these three species map onto in ",
                  "real species, see CBM_speciesCodes"),
    createsOutput(objectName = "CBM_speciesCodes", objectClass = "data.table",
                  "An object with 3 columns: pixelGroup, speciesCode, and Sp. This provides the species ",
                  "mapping for the CBM_AGB object which only specifies Sp1, Sp2, and Sp3")
  )
))


doEvent.Biomass_yieldTables = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$paths <- paths(sim)
      if (!is.null(Par$moduleNameAndBranch)) {
        mod$paths$modulePath <- file.path(dataPath(sim), "module")
      }
      sim <- scheduleEvent(sim, time(sim), "Biomass_yieldTables", "generateData",
                           eventPriority = -1)
      sim <- scheduleEvent(sim, time(sim), "Biomass_yieldTables", "generateYieldTables",
                           eventPriority = -1)
      sim <- scheduleEvent(sim, time(sim), "Biomass_yieldTables", "plotYieldTables",
                           eventPriority = -1)

    },
    generateData = {
      biomassCoresOuts <- runBiomass_core(Par$moduleNameAndBranch, mod$paths, sim$cohortData, sim$species,
                                          simEnv = envir(sim))
      sim$simOutputs <- biomassCoresOuts$simOutputs
      mod$digest <- biomassCoresOuts$digest
    },
    generateYieldTables = {

      message("Loading in cohortData files")
      cohortDataAll <- Cache(ReadExperimentFiles, omitArgs = "factorialOutputs",
                             .cacheExtra = mod$digest$outputHash, as.data.table(sim$simOutputs)[saved == TRUE])  # function already exists
      message("Converting to CBM Growth Increment ... This may take several minutes")
      cdObjs <- Cache(generateYieldTables, .cacheExtra = mod$digest$outputHash, cohortDataAll, numSpeciesKeep = 3,
                      omitArgs = c("cohortData"))
      sim$CBM_AGB <- cdObjs$cdWide
      sim$CBM_speciesCodes <- cdObjs$cdSpeciesCodes
      rm(cdObjs, cohortDataAll)
      gc()
    },
    plotYieldTables = {
      Plots(AGB = sim$CBM_AGB, sp = sim$CBM_speciesCodes, usePlot = FALSE, fn = pltfn,
            numPlots = Par$numPlots,
            ggsaveArgs = list(width = 10, height = 7),
            filename = paste("Yield Curves from", Par$numPlots,
                             "random plots -", gsub(":", "_", sim$._startClockTime)))
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}



# setkey(cds, pixelGroup, age)
generateYieldTables <- function(cohortData, numSpeciesKeep = 3) {
  cds <- cohortData
  setkeyv(cds, c("speciesCode", "pixelGroup"))

  # Because LandR biomass will lump all age < 11 into age 0
  if (sum(cds$age[cds$pixelGroup == 1] == 0) == 11)
    cds[age == 0, age := 0:10, by = c("pixelGroup", "speciesCode")]
  # Fix the age = 0 problem
  cds[, maxB := max(B), by = c("pixelGroup", "speciesCode")]
  set(cds, NULL, "maxB", as.integer(cds$maxB))

  # Sort them by maxB, in reverse order -- so first one in the list is largest maxB
  setorderv(cds, c("maxB"), order = -1L)
  suppressWarnings(set(cds, NULL, "Sp", NULL))
  # The next line is an efficient shortcut to getting a unique Sp1 per speciesCode within pixelGroup
  #  However, it can fail when two species have exactly the same maxB. So, subsequent line
  #  checks for failures (manifest by having duplicate Sp1 codes with two age = 1, 2 etc.)
  cds[, Sp1 := as.integer(factor(-maxB)), by = c("pixelGroup")]
  dd <- duplicated(cds, by = c("pixelGroup", "age", "Sp1"))
  if (any(dd)) {
    pgsWithDups <- unique(cds$pixelGroup[dd])
    cdDups <- cds[pixelGroup %in% pgsWithDups]
    corrections <- cdDups[, list(Sp1 = seq_along(unique(speciesCode)),
                                 speciesCode = unique(speciesCode)), by = "pixelGroup"]
    set(cdDups, NULL, "Sp1", NULL)
    corrections <- cdDups[corrections, on = c("pixelGroup", "speciesCode")]
    cds <- rbindlist(list(cds[!(pixelGroup %in% pgsWithDups)], corrections))
  }

  cds[, Sp := paste0("Sp", Sp1)]
  cds <- cds[Sp %in% paste0("Sp", 1:numSpeciesKeep)] # keep only most abundant X species
  cdSpeciesCodes <- cds[, list(Sp = Sp[1]), by = c("speciesCode", "pixelGroup")]
  set(cds, NULL, c("Sp1", "maxB", "speciesCode"), NULL)

  # Convert to wide format
  cdWide <- dcast(cds, pixelGroup + age ~ Sp, value.var = "B")
  setnames(cdWide, old = "pixelGroup", new = "id")

  # Convert NAs to zeros
  for (column in colnames(cdWide)) {
    if (anyNA(cdWide[[column]])) {
      set(cdWide, which(is.na(cdWide[[column]])), column, 0L)
    }
  }
  list(cdWide = cdWide, cdSpeciesCodes = cdSpeciesCodes)
}

runBiomass_core <- function(moduleNameAndBranch, paths, cohortData, species, simEnv) {
  # Get modules if using stand alone module
  if (!is.null(moduleNameAndBranch)) {
    getModule(moduleNameAndBranch, modulePath = paths$modulePath, overwrite = TRUE) # will only overwrite if wrong version
  }
  cohortDataForYield <- Copy(cohortData)
  cohortDataForYield$B <- 1L
  cohortDataForYield$age <- 0L
  timesForYield <- list(start = 0, end = max(species$longevity))

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
  opts <- options("LandR.assertions" = FALSE)
  on.exit(options(opts))
  parameters <- list(
    .globals = list(
      "sppEquivCol" = speciesNameConvention
    ),
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
  simOutputs <- Cache(simInitAndSpadesClearEnv,
                      paths = paths,
                      times = timesForYield,
                      params = parameters,
                      modules = "Biomass_core",
                      outputs = simOutputs,
                      objects = objects,
                      debug = 1, omitArgs = c("objects", "times", "debug"), .cacheExtra = dig
  )
  list(simOutputs = simOutputs, digest = dig)
}

### add additional events as needed by copy/pasting from above
ReadExperimentFiles <- function(factorialOutputs) {

  factorialOutputs <- as.data.table(factorialOutputs)[objectName == "cohortData"]
  fEs <- .fileExtensions()
  cdsList <- by(factorialOutputs, factorialOutputs[, "saveTime"], function(x) {
    fE <- reproducible:::fileExt(x$file)
    wh <- fEs[fEs$exts %in% fE,]
    message(crayon::green("reading: "))
    cat(crayon::green(x$file, "..."))
    cd <- getFromNamespace(wh$fun, ns = asNamespace(wh$package))(x$file)[, .(speciesCode, age, B, pixelGroup)]
    cat(crayon::green(" Done!\n"))
    return(cd)
  })
  gc() # need to clear memory
  message("rbindlisting the cohortData objects")
  cds <- rbindlist(cdsList, use.names = TRUE, fill = TRUE)

  return(invisible(cds))
}

simInitAndSpadesClearEnv <- function(...) {
  simOut <- simInitAndSpades(...)
  rm(list = ls(simOut, all.names = TRUE), envir = envir(simOut))
  outputs(simOut)
}

pltfn <- function(AGB, sp, numPlots) {
  pullOutId <- sample(1:max(AGB$id), size = numPlots)
  id2 <- AGB[id %in% pullOutId]
  id2melt <- melt(id2, variable.name = "Sp", measure = patterns("Sp"), value.name = "AGB")
  sp <- sp[pixelGroup %in% pullOutId]
  id2melt <- id2melt[sp, on = c("Sp", "id" = "pixelGroup")]
  gg <- ggplot(id2melt, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~id)
  return(invisible(gg))
}
