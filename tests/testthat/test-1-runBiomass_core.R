test_that("function runBiomass_core works", {
  Require::Require(c("SpaDES.core", "SpaDES.project"))
  
  # This runs the function runBiomass_core with a simList created by 
  # the module biomass_borealDataPrep.
  # The study area is a ~10km^2 a located in Northeast BC
  
  # Created with:
  
  # out <- SpaDES.project::setupProject(
  #   Restart = TRUE,
  #   paths = list(projectPath = getwd()),
  #   options = options(spades.moduleCodeChecks = FALSE,
  #                     spades.recoveryMode = FALSE),
  #   modules = c("PredictiveEcology/Biomass_borealDataPrep@development"
  #   ),
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
  #   packages = c("googledrive", 'RCurl', 'XML', "stars"),
  #   useGit = F,
  #   studyArea = {
  #     reproducible::prepInputs(url = "https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing",
  #                              destinationPath = "inputs",
  #                              fun = sf::st_read,
  #                              overwrite = TRUE) |>
  #       terra::vect() |>
  #       terra::crop(terra::ext(1127000, 1130000, 1267000, 1270000)) |>
  #       terra::aggregate()
  #   },
  #   studyAreaLarge = studyArea,
  #   sppNameVector = c("Abie_las", "Popu_tre")
  # )
  
  simOut <- SpaDES.core::loadSimList(file.path(test_path(), "fixtures", "smallSimOut.zip"),
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
