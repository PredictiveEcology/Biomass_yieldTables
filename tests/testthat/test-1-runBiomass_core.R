test_that("runBiomass_core works", {
  Require::Require(c("SpaDES.core", "SpaDES.project"))
  # get inputs
  # This runs the module with a simList created by the module biomass_borealDataPrep.
  # The study area is a 9km^2 a located in Northeast BC
  # Created with:
  # SpaDES.project::setupProject(
  #   Restart = TRUE,
  #   functions = "../Yield/R/getRIA.R",
  #   updateRprofile = TRUE,
  #   paths = list(projectPath = getwd(),
  #                inputPath = "inputs",
  #                outputPath = file.path("outputs"),
  #                cachePath = "cache"),
  #   modules = c("PredictiveEcology/Biomass_borealDataPrep@development"),
  #   #note scfm is a series of modules on a single git repository
  #   params = list(
  #     .globals = list(
  #       dataYear = 2011, #will get kNN 2011 data, and NTEMS 2011 landcover
  #       sppEquivCol = "LandR",
  #       .plots = c("png"),
  #       .studyAreaName = "tiny",
  #       .useCache = c(".inputObjects", "init")
  #     ),
  #     Biomass_core = list(.plotInterval = 25)
  #   ),
  #   options = list(#spades.allowInitDuringSimInit = TRUE,
  #     spades.allowSequentialCaching = TRUE,
  #     spades.moduleCodeChecks = FALSE,
  #     spades.recoveryMode = 1,
  #     LandR.verbose = TRUE #for regen messages
  #   ),
  #   packages = c('RCurl', 'XML', 'snow', 'googledrive'),
  #   times = list(start = 2011, end = 2011),
  #   useGit = F,
  #   studyArea = {
  #     reproducible::prepInputs(url = "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing",
  #                              destinationPath = "inputs",
  #                              overwrite = TRUE) |>
  #       terra::vect() |>
  #       terra::crop(terra::ext(1127000, 1130000, 1267000, 1270000)) |>
  #       terra::aggregate()
  #   },
  #   studyAreaLarge = studyArea,
  #   sppNameVector = c("Abie_las", "Pice_mar"),
  #   rasterToMatch = {
  #     rtm <- terra::rast(studyArea, res = c(250, 250))
  #     rtm[] <- 1
  #     rtm <- terra::mask(rtm, studyArea)
  #     rtm
  #   }
  # )
  
  
  simOut <- SpaDES.core::loadSimList(file.path(test_path(), "testdata", "smallSimOut.zip"),
                                     projectPath = file.path(testDirs$temp$projects, "5-Biomass_borealDataPrep"))

  
  out <- runBiomass_core(moduleNameAndBranch ="PredictiveEcology/Biomass_core@development", 
                         paths = list(
                           modulePath  = testDirs$temp$modules,
                           inputPath   = testDirs$temp$inputs,
                           outputPath = testDirs$temp$outputs
                         ),
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         simEnv = envir(simOut))
  
  # inspect output class
  expect_is(out, "list")
  expect_equal(names(out), c("simOutputs", "digest"))
  expect_is(out$simOutputs, "data.frame")
  expect_is(out$digest, "list")
  expect_equal(names(out$digest), c("outputHash", "preDigest"))

  #inspect simOutputs
  expect_true(all(out$simOutputs$objectName == "cohortData"))
  expect_true(all(out$simOutputs$saveTime == c(1:nrow(out$simOutputs)-1)))
  expect_true(all(out$simOutputs$saved))
  expect_true(nrow(out$simOutputs) == max(simOut$species$longevity)+1)
  
})
