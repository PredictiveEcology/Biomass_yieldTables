test_that("module runs with small example", {
  # packages = c("SpaDES.core", "SpaDES.project", "terra")
  # init.test.packages(packages)
  
  # Restore paths on teardown
  pathsOriginal <- list(wd = getwd(), libs = .libPaths())
  withr::defer({
    setwd(pathsOriginal$wd)
    #.libPaths(pathsOriginal$libs)
  })
  
  
  # This runs the module with a simList created by the modules biomass_borealDataPrep.
  
  # The study area is a ~10 km^2 a located in Northeast BC with a 250m resolution
  # The data is created with:
  
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
  
  module <- "Biomass_yieldTables"
  simOut <- SpaDES.core::loadSimList(file.path(test_path(), "fixtures", "smallSimOut.zip"),
                                     projectPath = file.path(testDirs$temp$projects, "5-Biomass_borealDataPrep"))
  outs <- lapply(objects(simOut), function(x) simOut[[x]])
  names(outs) <- objects(simOut)
  
  simInitInput <- .SpaDESwithCallingHandlers(
    SpaDES.project::setupProject(
      
      modules = module,
      paths   = list(
        projectPath = file.path(testDirs$temp$projects, "5-Biomass_yieldTables"),
        modulePath  = testDirs$temp$modules,
        inputPath   = testDirs$temp$inputs,
        outputPath = testDirs$temp$outputs
      ),
      params = list(
        #.progress = list(type = graphical, interval = 1),
        .globals = list(verbose = FALSE),
        Biomass_yieldTables = list(.saveInitialTime = NA)
      ),
      objects = outs
    )
  )
  
  simTestInit <- .SpaDESwithCallingHandlers(
    SpaDES.core::simInit2(simInitInput)
  )
  
  # is output a simList?
  expect_s4_class(simTestInit, "simList")
  
  simTest <- .SpaDESwithCallingHandlers(
    SpaDES.core::spades(simTestInit, debug = FALSE)
  )
  
  # is output a simList?
  expect_s4_class(simTest, "simList")
  
  # does output have your module in it
  expect_true(any(unlist(modules(simTest)) %in% module))
  
  # check output CBM_AGB
  expect_true(!is.null(simTest$CBM_AGB))
  expect_true(inherits(simTest$CBM_AGB, "data.table"))
  
  expected_colnames <- c("pixelGroup", "age", "B", "cohort_id")
  expect_true(all(names(simTest$CBM_AGB) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(simTest$CBM_AGB)))
  
  expect_type(simTest$CBM_AGB$pixelGroup, "integer")
  expect_type(simTest$CBM_AGB$age, "integer")
  expect_type(simTest$CBM_AGB$B, "integer")
  expect_type(simTest$CBM_AGB$cohort_id, "integer")
  
  expect_true(anyDuplicated(simTest$CBM_AGB[,c("age", "cohort_id")]) == 0)
  
  # check output CBM_speciesCodes
  expect_true(!is.null(simTest$CBM_speciesCodes))
  expect_true(inherits(simTest$CBM_speciesCodes, "data.table"))
  
  expected_colnames <- c("pixelGroup", "cohort_id", "speciesCode")
  expect_true(all(names(simTest$CBM_speciesCodes) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(simTest$CBM_speciesCodes)))
  
  expect_type(simTest$CBM_speciesCodes$pixelGroup, "integer")
  expect_type(simTest$CBM_speciesCodes$cohort_id, "integer")
  expect_is(simTest$CBM_speciesCodes$speciesCode, "factor")
  
  expect_true(anyDuplicated(simTest$CBM_speciesCodes$cohort_id) == 0)
  
  # unload.test.packages(packages)
  
})
