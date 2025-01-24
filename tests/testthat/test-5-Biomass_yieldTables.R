test_that("module runs with small example", {
  library(SpaDES.core)
  library(SpaDES.project)
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
  sim <- SpaDES.core::loadSimList(file.path(test_path(), "testdata", "smallSimOut.zip"), 
                                  projectPath = file.path(testDirs$temp$projects, "5-Biomass_borealDataPrep"))

  simInitInput <- .SpaDESwithCallingHandlers(
    SpaDES.project::setupProject(
      
      modules = "Biomass_yieldTables",
      paths   = list(
        projectPath = file.path(testDirs$temp$projects, "5-Biomass_yieldTables"),
        modulePath  = testDirs$temp$modules,
        inputPath   = testDirs$temp$inputs
      ),
      params = list(
        #.progress = list(type = graphical, interval = 1),
        .globals = list(verbose = FALSE),
        Biomass_yieldTables = list(.saveInitialTime = NA)
      ),
      require = c("testthat", "SpaDES.project", "SpaDES.core", "LandR"),
      studyArea = sim$studyArea,
      cohortData = sim$cohortData,
      species = sim$species
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
  expect_true(any(unlist(modules(simTest)) %in% c(unlist(module))))

  # did it run to the end?
  expect_true(time(simTest) == max(simTest$species$longevity))

  # check output CBM_AGB
  expect_true(!is.null(simTest$CBM_AGB))
  expect_true(inherits(simTest$CBM_AGB, "data.table"))
  
  expected_colnames <- c("pixelGroup", "age", "B", "cohort_id")
  expect_true(all(names(simTest$CBM_AGB) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(simTest$CBM_AGB)))
  
  expect_type(simTest$CBM_AGB$pixelGroup, "integer")
  expect_type(simTest$CBM_AGB$age, "integer")
  expect_type(simTest$CBM_AGB$B, "numeric")
  expect_type(simTest$CBM_AGB$cohort_id, "integer")
  
  expect_true(anyDuplicated(simTest$CBM_AGB[,c("age", "cohort_id")]) == 0)
  
  # check output CBM_speciesCodes
  expect_true(!is.null(simTest$CBM_speciesCodes))
  expect_true(inherits(simTest$CBM_speciesCodes, "data.table"))
  
  expected_colnames <- c("pixelGroup", "cohort_id", "SpeciesCode")
  expect_true(all(names(simTest$CBM_AGB) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(simTest$CBM_AGB)))
  
  expect_type(simTest$CBM_AGB$pixelGroup, "integer")
  expect_type(simTest$CBM_AGB$cohort_id, "integer")
  expect_type(simTest$CBM_AGB$SpeciesCode, "character")
  
  expect_true(anyDuplicated(simTest$CBM_AGB$cohort_id) == 0)
})
