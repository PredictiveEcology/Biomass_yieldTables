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
  reqdPkgs = list("data.table", "PredictiveEcology/SpaDES.core@development (>= 1.0.9.9008)", "LandR", "terra"),
  parameters = rbind(
    defineParameter(".useCache", "character", c("generateData", "generateYieldTables"), NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 40, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted"),
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
                             paths = mod$paths, cohortData = sim$cohortData,
                             species = sim$species, simEnv = envir(sim))
  sim$yieldOutputs <- biomassCoresOuts$simOutputs
  mod$pixelGroupRef <- biomassCoresOuts$pixelGroupRef
  mod$digest <- biomassCoresOuts$digest
  return(sim)
}

GenerateYieldTables <- function(sim) {
  message("Simulation done! Loading in cohortData files")
  cohortDataAll <- Cache(ReadExperimentFiles, omitArgs = "factorialOutputs",
                         .cacheExtra = mod$digest$outputHash, as.data.table(sim$yieldOutputs)[saved == TRUE])  # function already exists
  message("Converting to CBM Growth Increment ... This may take several minutes")
  cdObjs <- Cache(generateYieldTables, .cacheExtra = mod$digest$outputHash, cohortDataAll, pixelGroupRef = mod$pixelGroupRef, omitArgs = c("cohortData"))
  sim$CBM_AGB <- cdObjs$cds
  sim$CBM_speciesCodes <- cdObjs$cdSpeciesCodes
  rm(cdObjs, cohortDataAll)
  gc()
  return(sim)
}

PlotYieldTables <- function(sim) {
  fname = paste("Yield Curves from", Par$numPlots,
                "random plots -", gsub(":", "_", sim$._startClockTime))
  Plots(AGB = sim$CBM_AGB, sp = sim$CBM_speciesCodes, usePlot = FALSE, fn = pltfn,
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

