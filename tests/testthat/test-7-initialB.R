## Tables start as Biomass_core starts new cohorts (age 1, biomass initialB), with an
## age-0 row of biomass 0 added for consumers (helper-yieldTableContract.R).

test_that("initYieldCohorts starts cohorts at age 1 and initialB", {
  cohortData <- data.table::data.table(
    speciesCode = factor(c("Abie_las", "Popu_tre", "Abie_las")),
    pixelGroup = c(1L, 1L, 2L),
    age = c(110L, 80L, 50L),
    B = c(4200L, 3000L, 900L)
  )
  out <- initYieldCohorts(cohortData, initialB = 10)
  expect_identical(out$B, rep(10L, 3))
  expect_identical(out$age, rep(1L, 3))
  expect_identical(out$pixelGroup, cohortData$pixelGroup)
  expect_identical(cohortData$age, c(110L, 80L, 50L)) # input is not modified
})

test_that("addAgeZeroRows gives every table an age-0 row of biomass 0 and keeps the contract", {
  yt <- data.table::data.table(yieldTableIndex = 1L, speciesCode = factor(rep(c("Pice_eng", "Abie_las"), each = 3)),
                               age = rep(1:3, 2), biomass = c(10L, 30L, 90L, 10L, 25L, 80L))
  expect_identical(yieldTableViolations(yt), "no age-0 row")
  out <- addAgeZeroRows(yt)
  expect_identical(nrow(out), nrow(yt) + 2L)
  expect_identical(out[age == 0L]$biomass, c(0L, 0L))
  expect_identical(out[age == 1L]$biomass, c(10L, 10L))
  expect_identical(yieldTableViolations(out, maxAge = 3L), character(0))
})

test_that("yield tables from Biomass_core start at age 1 and initialB, with consecutive ages", {
  skip_on_cran()
  skip_if_offline() # runBiomass_core() downloads Biomass_core from GitHub

  simOut <- loadSmallSim()
  paths <- list(
    modulePath = file.path(dirname(testPaths$inputPath), "submodules"),
    inputPath  = testPaths$inputPath,
    outputPath = testPaths$outputPath
  )
  out <- runBiomass_core(moduleNameAndBranch = "PredictiveEcology/Biomass_core@main",
                         paths = paths,
                         cohortData = simOut$cohortData,
                         species = simOut$species,
                         maxAge = 5,
                         simEnv = envir(simOut),
                         initialB = 25)
  yt <- addAgeZeroRows(ReadExperimentFiles(out$simOutputs))
  expect_true(all(yt[age == 1L]$biomass == 25L))
  expect_true(all(yt[age == 0L]$biomass == 0L))
  expectYieldTableContract(yt, maxAge = 5L)
  expect_identical(max(yt$age), 5L)
})
