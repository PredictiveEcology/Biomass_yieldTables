test_that("module runs with small example", {
  module <- list("Biomass_yieldTables")
  path <- list(
    modulePath  = testDirs$temp$modules,
    inputPath   = testDirs$temp$inputs
  )
  parameters <- list(
    #.progress = list(type = graphical, interval = 1),
    .globals = list(verbose = FALSE),
    Biomass_yieldTables = list(.saveInitialTime = NA)
  )

  # This is a simList created by the module biomass_borealDataPrep.
  # The study area is a XX a located in Northeast BC
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
  #                              fun = terra::vect(),
  #                              overwrite = TRUE) |>
  #       terra::crop(terra::ext(1130000, 1140000, 1250000, 1260000)) |>
  #       terra::aggregate()
  #   },
  #   studyAreaLarge = studyArea,
  #   rasterToMatch = {
  #     rtm <- terra::rast(studyArea, res = c(250, 250))
  #     rtm[] <- 1
  #     rtm <- terra::mask(rtm, studyArea)
  #     rtm
  #   }
  # )
  
  objects <- readRDS(file.path(test_path(), "testdata", "smallSimOut.RDS"))
  
  mySim <- .SpaDESwithCallingHandlers(
    simInit(params = parameters,
            modules = module,
            objects = objects,
            paths = path)
  )
  
  # is output a simList?
  expect_s4_class(output, "simList")
  
  output <- .SpaDESwithCallingHandlers(
    spades(mySim, debug = FALSE)
  )
  
  # is output a simList?
  expect_s4_class(output, "simList")

  # does output have your module in it
  expect_true(any(unlist(modules(output)) %in% c(unlist(module))))

  # did it run to the end?
  expect_true(time(output) == max(objects$species$longevity))

  # check output CBM_AGB
  expect_true(!is.null(output$CBM_AGB))
  expect_true(inherits(output$CBM_AGB, "data.table"))
  
  expected_colnames <- c("pixelGroup", "age", "B", "cohort_id")
  expect_true(all(names(output$CBM_AGB) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(output$CBM_AGB)))
  
  expect_type(output$CBM_AGB$pixelGroup, "integer")
  expect_type(output$CBM_AGB$age, "integer")
  expect_type(output$CBM_AGB$B, "numeric")
  expect_type(output$CBM_AGB$cohort_id, "integer")
  
  expect_true(anyDuplicated(output$CBM_AGB[,c("age", "cohort_id")]) == 0)
  
  # check output CBM_speciesCodes
  expect_true(!is.null(output$CBM_speciesCodes))
  expect_true(inherits(output$CBM_speciesCodes, "data.table"))
  
  expected_colnames <- c("pixelGroup", "cohort_id", "SpeciesCode")
  expect_true(all(names(output$CBM_AGB) %in% expected_colnames))
  expect_true(all(expected_colnames %in% names(output$CBM_AGB)))
  
  expect_type(output$CBM_AGB$pixelGroup, "integer")
  expect_type(output$CBM_AGB$cohort_id, "integer")
  expect_type(output$CBM_AGB$SpeciesCode, "character")
  
  expect_true(anyDuplicated(output$CBM_AGB$cohort_id) == 0)
})
