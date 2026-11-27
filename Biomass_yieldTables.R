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
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut")),
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com", role = c("ctb")),
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(Biomass_yieldTables = "0.0.8.9003"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_yieldTables.Rmd")), ## same file
  reqdPkgs = list("crayon", "data.table", "digest", "ggplot2", "PredictiveEcology/LandR@development", "reproducible",
                  "PredictiveEcology/SpaDES.project@development",
                  "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)", "terra",
                  ## Biomass_core, which this module runs internally, needs these too
                  "assertthat", "dplyr", "fpCompare", "purrr", "quickPlot", "Rcpp", "R.utils",
                  "scales", "SpaDES.tools", "tidyr", "ianmseddy/LandR.CS@master",
                  "PredictiveEcology/pemisc@development"),
  parameters = rbind(
    defineParameter(".useCache", "character", c("generateData", "generateYieldTables"), NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 40L, NA, NA,
                    paste("Total number of yield tables for which curves will be plotted: the most",
                          "prevalent (most pixels) ones within each of the most prevalent `ecoregionGroup`s.")),
    defineParameter("numEcoregionGroups", "integer", 4L, 1L, NA,
                    paste("Number of the most prevalent `ecoregionGroup`s (most pixels) whose yield curves are",
                          "plotted. `numPlots` is split evenly among them.")),
    defineParameter("headstartYears", "integer", 5L, 0L, NA,
                    paste("If above 0 (set 0 to turn off), `yieldTablesCumulativeHeadstart` is also created: species whose",
                          "`postfireregen` is in `headstartRegen` grow alone for this many years before the other species are",
                          "added at age 1 with `initialB`. This represents a landscape after fire, where seed bank and",
                          "resprouting species start first. Only `yieldTableIndex`es with at least one such species and at",
                          "least two species differ from `yieldTablesCumulative`.")),
    defineParameter("headstartRegen", "character", c("serotiny", "resprout"), NA, NA,
                    "Values of `species$postfireregen` that get the head start (see `headstartYears`)."),
    defineParameter("maxAge", "integer", NA, NA, NA,
                    "The number of years for which the yield tables are created. If not provided, ",
                    "the yield tables will be created to the largest species longevity."),
    defineParameter("initialB", "numeric", 10, 1, NA,
                    paste("Biomass (g/m2) of each table at age 1. Cohorts start as Biomass_core starts new",
                          "cohorts (age 1, biomass `initialB`), and this is passed to Biomass_core as its",
                          "`initialB`; the default matches Biomass_core's. Every table also has an age-0",
                          "row with biomass 0.")),
    defineParameter("moduleNameAndBranch", "character", "PredictiveEcology/Biomass_core@development (>= 1.3.9)", NA, NA,
                    "The branch and version number required for Biomass_core. This will be downloaded ",
                    "into the 'submodules' folder of this module, so it does not ",
                    "interact with the main user's modules."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If NA, a hash of studyArea will be used.")
  ),
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`) with the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age`",
                              "(integer in years). Must be supplied by the user or created in another module",
                              "like `Biomass_BiomassDataPrep`.")),
    expectsInput("species", "data.table",
                 desc = paste("A table of invariant species traits with the following trait colums:",
                              "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
                              "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
                              "'mortalityshape', 'growthcurve', 'resproutage_min', 'resproutage_max',",
                              "'postfireregen', 'wooddecayrate', 'leaflongevity' 'leafLignin',",
                              "'hardsoft'. The last seven traits are not used in *Biomass_core*,",
                              "and may be ommited. However, this may result in downstream issues with",
                              "other modules. Must be supplied by the user or created in another module",
                              "like `Biomass_BiomassDataPrep`.")),
    expectsInput("sppColorVect", "character",
                 desc = paste("Optional. Named character vector of hex colours, one per species (names are `speciesCode`s),",
                              "used to plot the yield curves. Created by `Biomass_borealDataPrep`; if absent, ggplot's",
                              "default colours are used.")),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "template raster to use for simulations; defaults to RIA study area")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "yieldTablesCumulative", objectClass = "data.table",
                  paste("Yield Tables intended to supply the requirements for a CBM spinup.",
                        "Columns are `yieldTableIndex`, `age`, `speciesCode`, `biomass`.",
                        "`yieldTableIndex` is the growth curve identifier that depends",
                        "on species combination. `biomass` is the biomass for the",
                        "given species at the pixel age.")),
    createsOutput(objectName = "yieldTablesId", objectClass = "data.table",
                  "A data.table linking spatially the `yieldTableIndex`. Columns are `pixelIndex` and `yieldTableIndex`"),
    createsOutput(objectName = "yieldTablesCumulativeHeadstart", objectClass = "data.table",
                  paste("Only if `headstartYears` > 0. Same `yieldTableIndex`es (and `yieldTablesId`) as `yieldTablesCumulative`,",
                        "but species with `postfireregen` in `headstartRegen` grow alone for `headstartYears` years. Here `age`",
                        "is the age of the first cohorts (years since the disturbance + 1) for all species, so the other species",
                        "have no rows before `headstartYears` + 1 and start there with `initialB`; it does not follow the age-0",
                        "contract of `yieldTablesCumulative`. Tables without such a species are copied from",
                        "`yieldTablesCumulative`."))
  )
))


doEvent.Biomass_yieldTables = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$paths <- paths(sim)
      if (!is.null(Par$moduleNameAndBranch)) {
        mod$paths$modulePath <- file.path(modulePath(sim)[1], currentModule(sim), "submodules")
      }
      sim <- GenerateData(sim)
      
      sim <- GenerateYieldTables(sim)
      
      sim <- PlotYieldTables(sim)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

GenerateData <- function(sim) {
  message("Running simulations for all PixelGroups")
  biomassCoresOuts <-  Cache(runBiomass_core, moduleNameAndBranch = Par$moduleNameAndBranch,
                             paths = mod$paths, cohortData = sim$cohortData, maxAge = Par$maxAge,
                             species = sim$species, simEnv = envir(sim),
                             initialB = Par$initialB, headstartYears = Par$headstartYears,
                             headstartRegen = Par$headstartRegen,
                             omitArgs = "simEnv")
  mod$yieldOutputs <- biomassCoresOuts$simOutputs
  mod$headstart <- biomassCoresOuts$headstart
  sim$yieldTablesId <- data.table(
    yieldTableIndex = as.integer(biomassCoresOuts$yieldPixelGroupMap[])
  )
  ####
  windowSize <- 3L
  i <- 1L
  while(any(sim$yieldTablesId$yieldTableIndex == 0, na.rm = T) && windowSize <= 10){
    Npix <- sum(sim$yieldTablesId$yieldTableIndex == 0, na.rm = T)
    message("Filling empty forest pixels: ", Npix, " pixels to fill.")
    message("Using window size = ", windowSize)
    # replace 0 by NA
    x <- biomassCoresOuts$yieldPixelGroupMap
    x[x == 0] <- NA
    focaledYldPixGrMap <- focal(x, w = windowSize, fun = "modal", na.rm = TRUE, na.policy="only")
    newClasses <- focaledYldPixGrMap[sim$yieldTablesId$yieldTableIndex == 0]
    idToReplace <- !is.na(sim$yieldTablesId$yieldTableIndex) & sim$yieldTablesId$yieldTableIndex == 0 & !is.na(focaledYldPixGrMap[])
    sim$yieldTablesId[idToReplace, ] <- newClasses[!is.na(newClasses)]
    biomassCoresOuts$yieldPixelGroupMap[idToReplace] <- focaledYldPixGrMap[idToReplace]
    if(i %% 3 == 0) {windowSize <- windowSize + 2}
    i <- i + 1L
  }
  ####
  sim$yieldTablesId <- sim$yieldTablesId[, pixelIndex := .I] |> na.omit()
  setcolorder(sim$yieldTablesId, c("pixelIndex", "yieldTableIndex"))
  mod$digest <- biomassCoresOuts$digest
  ## pixels per yield table and its ecoregionGroup, to plot the most prevalent ones
  mod$prevalence <- merge(sim$yieldTablesId[, .(nPixels = .N), by = "yieldTableIndex"],
                          biomassCoresOuts$yieldEcoregion, by = "yieldTableIndex")
  return(sim)
}

GenerateYieldTables <- function(sim) {
  message("Simulation done! Loading in cohortData files")
  cohortDataAll <- Cache(ReadExperimentFiles, omitArgs = "factorialOutputs",
                         .cacheExtra = mod$digest$outputHash, as.data.table(mod$yieldOutputs)[saved == TRUE])
  cohortDataAll <- addAgeZeroRows(cohortDataAll)
  sim$yieldTablesCumulative <- cohortDataAll
  setcolorder(sim$yieldTablesCumulative, c("yieldTableIndex", "speciesCode", "age", "biomass"))
  rm(cohortDataAll)
  if (!is.null(mod$headstart)) {
    sim$yieldTablesCumulativeHeadstart <- headstartTables(mod$headstart, sim$yieldTablesCumulative)
    unlink(c(mod$headstart$phase1$file, mod$headstart$phase2$file)) # the tables are in memory now
  }
  gc()
  return(sim)
}

readHeadstartPhase <- function(outputs, digest) {
  ReadExperimentFiles(as.data.table(outputs)[saved == TRUE], keepTime = TRUE)
}

## phase 1 (head-start species alone) up to the year the others join, then phase 2. As in the plain tables,
## the cohorts are age 1 at time 0, so `age` is the time + 1
headstartTables <- function(headstart, plainTables, read = readHeadstartPhase) {
  phase1 <- read(headstart$phase1, headstart$digest[[1]]$outputHash)[year < headstart$years]
  phase2 <- read(headstart$phase2, headstart$digest[[2]]$outputHash)[year >= headstart$years]
  tables <- rbindlist(list(phase1, phase2), use.names = TRUE)
  tables[, age := as.integer(year) + 1L]
  tables[, year := NULL]
  untouched <- plainTables[!yieldTableIndex %in% headstart$groups]
  tables <- rbindlist(list(tables, untouched), use.names = TRUE)
  setcolorder(tables, c("yieldTableIndex", "speciesCode", "age", "biomass"))
  setkeyv(tables, c("yieldTableIndex", "speciesCode", "age"))
  tables
}

PlotYieldTables <- function(sim) {
  colours <- sim$sppColorVect
  fname = paste("Yield Curves from the most prevalent plots -", gsub(":", "_", sim$._startClockTime))
  Plots(data = sim$yieldTablesCumulative, usePlot = FALSE, fn = pltfn,
        numPlots = Par$numPlots, numEcoregionGroups = Par$numEcoregionGroups,
        prevalence = mod$prevalence, colours = colours,
        ggsaveArgs = list(width = 10, height = 7),
        filename = fname)
  if (!is.null(sim$yieldTablesCumulativeHeadstart)) {
    # one page of 8 examples (each with its plain counterpart), spread over the top ecoregion groups
    Plots(data = sim$yieldTablesCumulativeHeadstart, usePlot = FALSE, fn = pltfn,
          numPlots = 8L, numEcoregionGroups = Par$numEcoregionGroups,
          prevalence = mod$prevalence, plain = sim$yieldTablesCumulative,
          onlyTables = mod$headstart$groups, colours = colours,
          ggsaveArgs = list(width = 14, height = 9),
          filename = paste0("Yield Curves with a head start of ", Par$headstartYears, " years - ",
                            gsub(":", "_", sim$._startClockTime)))
  }
  compTables <- if (is.null(sim$yieldTablesCumulativeHeadstart)) sim$yieldTablesCumulative else sim$yieldTablesCumulativeHeadstart
  Plots(data = compTables, usePlot = FALSE, fn = pltfnCompetition,
        prevalence = mod$prevalence, n = 12L, colours = colours,
        ggsaveArgs = list(width = 14, height = 9),
        filename = paste("Species growing with competition -", gsub(":", "_", sim$._startClockTime)))
  mapRast <- rast(sim$rasterToMatch)
  mapRast[sim$yieldTablesId$pixelIndex] <- sim$yieldTablesId$yieldTableIndex
  Plots(mapRast, usePlot = TRUE, deviceArgs = list(width = 700, height = 500),
        filename = "yieldTableIdMap")
  return(sim)
}


## .inputObjects ------------------------------------------------------------------------------
.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    stop("Please provide a 'rasterToMatch' object")
  }
  
  if (!suppliedElsewhere("cohortData", sim)) {
    stop("Please provide a 'cohortData' table or use a module like Biomass_borealDataPrep")
  }
  
  if (!suppliedElsewhere("species", sim)) {
    stop("Please provide a 'species' table or use a module like Biomass_borealDataPrep")
  }
  
  return(invisible(sim))
}

