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
    person("Eliot", "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(Biomass_yieldTables = "0.0.8"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Biomass_yieldTables.Rmd")), ## same file
  reqdPkgs = list("data.table", "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)", "LandR", "terra"),
  parameters = rbind(
    defineParameter(".useCache", "character", c("generateData", "generateYieldTables"), NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 40L, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted"),
    defineParameter("maxAge", "integer", NA, NA, NA,
                    "The number of years for which the growth tables are created."),
    defineParameter("moduleNameAndBranch", "character", "PredictiveEcology/Biomass_core@development (>= 1.3.9)", NA, NA,
                    "The branch and version number required for Biomass_core. This will be downloaded ",
                    "into the file.path(dataPath(sim), 'module') of this module, so it does not ",
                    "interact with the main user's modules. If this is set to NULL, then ",
                    "this module will not download a new copy of Biomass_core, but will use ",
                    "the existing one in the modulePath(sim)"),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If NA, a hash of studyArea will be used.")
  ),
  ## DC, 22-01-2025
  ## For now, there are no default for these inputs.
  ## In theory, a user could provide any of the inputs of biomass_core.
  ## In the future, `runBiomass_core` could be modified to be more explicit about
  ## what it uses. The list of inputs should also be updated.
  inputObjects = bindrows(
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`) with the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age`",
                              "(integer in years). Must be supplied by the user or created in another module",
                              "like *biomass_BiomassDataPrep*.")),
    expectsInput("species", "data.table",
                 desc = paste("A table of invariant species traits with the following trait colums:",
                              "'species', 'Area', 'longevity', 'sexualmature', 'shadetolerance',",
                              "'firetolerance', 'seeddistance_eff', 'seeddistance_max', 'resproutprob',",
                              "'mortalityshape', 'growthcurve', 'resproutage_min', 'resproutage_max',",
                              "'postfireregen', 'wooddecayrate', 'leaflongevity' 'leafLignin',",
                              "'hardsoft'. The last seven traits are not used in *Biomass_core*,",
                              "and may be ommited. However, this may result in downstream issues with",
                              "other modules. Must be supplied by the user or created in another module",
                              "like *biomass_BiomassDataPrep*.")),
    expectsInput("studyArea", "sfc",
                 desc = paste("Polygon to use as the study area. Must be supplied by the user. Can also be a SpatVector."))
    
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "yieldOutputs", objectClass = "data.frame",
                  desc = "A data.frame showing the cohortData files that were created during this ",
                  "module. Normally, these can be deleted as they are re-read within this module ",
                  "and converted into CBM_AGB and CBM_speciesCodes."),
    createsOutput(objectName = "yieldTables", objectClass = "data.table",
                  "A large object intended to supply the eventual requirements for a CBM growth increment object. ",
                  "This one will have column names of yieldPixelGroup, age, cohort_id, B. The last ",
                  "column represent aboveground biomass of that cohort_id at that age. Note, these ",
                  "will not be strictly increasing as the species approaches its longevity. ",
                  "To see what these three species map onto in real species, see yieldSpeciesCodes"),
    createsOutput(objectName = "yieldSpeciesCodes", objectClass = "data.table",
                  "An object with 3 columns: yieldPixelGroup, cohort_id, and speciesCode. This provides the species ",
                  "mapping for the yieldTables object"),
    createsOutput(objectName = "yieldPixelGroupMap", objectClass = "spatRaster",
                 "A raster mapping yieldPixelGroup")
  )
))


doEvent.Biomass_yieldTables = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      mod$paths <- paths(sim)
      if (!is.null(Par$moduleNameAndBranch)) {
        mod$paths$modulePath <- file.path(modulePath(sim), currentModule(sim), "submodules")
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
                             species = sim$species, simEnv = envir(sim))
  sim$yieldOutputs <- biomassCoresOuts$simOutputs
  sim$yieldPixelGroupMap <- biomassCoresOuts$yieldPixelGroupMap
  mod$digest <- biomassCoresOuts$digest
  return(sim)
}

GenerateYieldTables <- function(sim) {
  message("Simulation done! Loading in cohortData files")
  cohortDataAll <- Cache(ReadExperimentFiles, omitArgs = "factorialOutputs",
                         .cacheExtra = mod$digest$outputHash, as.data.table(sim$yieldOutputs)[saved == TRUE])  # function already exists
  setnames(cohortDataAll, "pixelGroup", "yieldPixelGroup")
  message("Converting to CBM Growth Increment ... This may take several minutes")
  cdObjs <- Cache(generateYieldTables, .cacheExtra = mod$digest$outputHash, cohortDataAll, omitArgs = c("cohortData"))
  sim$yieldTables <- cdObjs$cds
  sim$yieldSpeciesCodes <- cdObjs$cdSpeciesCodes
  rm(cdObjs, cohortDataAll)
  gc()
  return(sim)
}

PlotYieldTables <- function(sim) {
  fname = paste("Yield Curves from", Par$numPlots,
                "random plots -", gsub(":", "_", sim$._startClockTime))
  Plots(AGB = sim$yieldTables, sp = sim$yieldSpeciesCodes, usePlot = FALSE, fn = pltfn,
        numPlots = Par$numPlots,
        ggsaveArgs = list(width = 10, height = 7),
        filename = fname)
  return(sim)
}


## .inputObjects ------------------------------------------------------------------------------
.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("studyArea", sim)) {
    stop("Please provide a 'studyArea' polygon")
  }
  
  if (!suppliedElsewhere("cohortData", sim)) {
    stop("Please provide a 'cohortData' table or use a module like Biomass_borealDataPrep")
  }
  
  if (!suppliedElsewhere("species", sim)) {
    stop("Please provide a 'species' table or use a module like Biomass_borealDataPrep")
  }
  
  return(invisible(sim))
}

