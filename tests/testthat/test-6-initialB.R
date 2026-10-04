test_that("initYieldCohorts starts cohorts at initialB and age 1, as Biomass_core does", {
  cohortData <- data.table(
    speciesCode = factor(c("Abie_las", "Popu_tre", "Abie_las")),
    pixelGroup = c(1L, 1L, 2L),
    age = c(110L, 80L, 50L),
    B = c(4200L, 3000L, 900L)
  )
  initialB <- 10
  out <- initYieldCohorts(cohortData, initialB)
  expect_true(all(out$B == initialB))
  expect_true(all(out$age == 1L))
  expect_type(out$B, "integer")
  expect_equal(out$pixelGroup, cohortData$pixelGroup)
  expect_equal(cohortData$age, c(110L, 80L, 50L)) # input is not modified
})

test_that("yield tables start at initialB and age 1", {
  skip_on_cran()
  skip_if_offline() # runBiomass_core() downloads Biomass_core from GitHub

  simOut <- loadSmallSim()
  paths <- list(
    modulePath = file.path(dirname(testPaths$inputPath), "submodules"),
    inputPath  = testPaths$inputPath,
    outputPath = testPaths$outputPath
  )
  initialB <- 10
  out <- runBiomass_core(moduleNameAndBranch = "PredictiveEcology/Biomass_core@main",
                         paths = paths,
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         maxAge = 3,
                         simEnv = envir(simOut),
                         initialB = initialB)
  yieldTables <- ReadExperimentFiles(out$simOutputs)
  firstYear <- yieldTables[age == min(age)]
  expect_true(all(firstYear$age == 1L))
  expect_true(all(firstYear$biomass == initialB))
  # ages count up from 1 with no age-0 rows to relabel
  expect_false(any(yieldTables$age == 0L))
  expect_true(all(yieldTables[, all(diff(sort(age)) == 1L), by = c("yieldTableIndex", "speciesCode")]$V1))
})
