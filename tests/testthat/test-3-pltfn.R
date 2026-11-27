test_that("function pltfn works", {
  #####
  # dummy inputs
  ngroup <- 3
  nage <- 5
  nsp <- 2
  AGB <- data.table::as.data.table(
    expand.grid(yieldTableIndex = c(1:ngroup),
                age = c(1:nage),
                speciesCode = as.factor(c(1:nsp)))
  )
  AGB$biomass <- AGB$age
  #####
  
  prevalence <- data.table::data.table(yieldTableIndex = 1:ngroup, ecoregionGroup = "5_210", nPixels = ngroup:1)

  # general
  expect_true(inherits(pltfn(AGB, 3, 1, prevalence), what = "ggplot"))
  expect_error(pltfn())
  expect_error(pltfn(AGB, 0, 1, prevalence))
  expect_invisible(pltfn(AGB, 2, 1, prevalence))
  expect_no_error(pltfn(AGB, 1, 1, prevalence))

  # number of facets
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 2, 1, prevalence))$data[[1]]$PANEL)), 2)
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 10, 1, prevalence))$data[[1]]$PANEL)), ngroup)
})

test_that("pltfn pairs plain and head-start tables, pages them and keeps only chosen tables", {
  mk <- function(f) data.table::data.table(
    yieldTableIndex = rep(1:4, each = 3), speciesCode = "a", age = rep(0:2, 4), biomass = f * rep(0:2, 4))
  plain <- mk(1)
  hs <- mk(2)
  prevalence <- data.table::data.table(yieldTableIndex = 1:4, ecoregionGroup = "5_210", nPixels = 4:1)

  gg <- pltfn(hs, numPlots = 4, numEcoregionGroups = 1, prevalence = prevalence, plain = plain)
  lev <- levels(gg$data$panel)
  expect_equal(lev[1:2], c("5_210--pg1 (plain)", "5_210--pg1 (head start)"))
  expect_length(lev, 8)

  page2 <- pltfn(hs, 4, 1, prevalence, plain = plain, page = 2, nPages = 2)
  expect_length(levels(page2$data$panel), 4)
  expect_false(any(grepl("pg1 |pg2 ", levels(page2$data$panel))))

  only <- pltfn(hs, 4, 1, prevalence, plain = plain, onlyTables = c(2, 4))
  expect_equal(sort(unique(only$data$yieldTableIndex)), c(2L, 4L))

  # head-start biomass is plotted as given, and the inputs are not modified
  expect_equal(max(gg$data[grepl("head start", panel), AGB]), 4)
  expect_true("biomass" %in% names(hs))
})

test_that("headstartTables uses phase 1 before the join year, phase 2 after, and plain tables elsewhere", {
  plainTables <- data.table::data.table(yieldTableIndex = c(1L, 1L, 2L, 2L), speciesCode = "a",
                                        age = c(0L, 1L, 0L, 1L), biomass = c(1, 5, 1, 7))
  phase1 <- data.table::data.table(yieldTableIndex = 1L, speciesCode = "p", year = 0:2, biomass = c(10, 20, 30))
  phase2 <- data.table::data.table(yieldTableIndex = 1L, speciesCode = c("p", "s"), year = 2L, biomass = c(31, 10))
  out <- headstartTables(
    list(phase1 = phase1, phase2 = phase2, years = 2L, groups = 1L,
         digest = list(list(outputHash = "a"), list(outputHash = "b"))),
    plainTables, read = function(outputs, digest) outputs)
  expect_equal(out[yieldTableIndex == 1 & speciesCode == "p", age], 1:3)
  expect_equal(out[yieldTableIndex == 1 & speciesCode == "p" & age == 3, biomass], 31)
  expect_equal(out[yieldTableIndex == 1 & speciesCode == "s", age], 3L)
  expect_equal(out[yieldTableIndex == 2, biomass], c(1, 7))
})

test_that("headstartGroups keeps multi-species groups with a head-start species", {
  cohortData <- data.table::data.table(
    pixelGroup = c(1, 1, 2, 3, 3, 4),
    speciesCode = factor(c("Pinu_con", "Pice_gla", "Pinu_con", "Pice_gla", "Abie_las", "Pice_gla")))
  expect_equal(headstartGroups(cohortData, "Pinu_con"), 1)
  expect_equal(headstartGroups(cohortData, c("Pinu_con", "Abie_las")), c(1, 3))
  expect_length(headstartGroups(cohortData, "Betu_pap"), 0)
})

test_that("pltfn uses the supplied species colours", {
  AGB <- data.table::data.table(yieldTableIndex = 1L, age = 1:3, speciesCode = "Pice_eng", biomass = 1:3)
  prevalence <- data.table::data.table(yieldTableIndex = 1L, ecoregionGroup = "5_210", nPixels = 1L)
  gg <- pltfn(AGB, 1, 1, prevalence, colours = c(Pice_eng = "#112233"))
  expect_true(inherits(gg$scales$get_scales("colour"), "ScaleDiscrete"))
})

test_that("pltfnCompetition covers every species with a competitor", {
  AGB <- data.table::CJ(yieldTableIndex = 1:30, speciesCode = c("A", "B", "C"), age = 1:5)
  AGB <- AGB[!(speciesCode == "C" & yieldTableIndex %% 3 != 0) & !(yieldTableIndex == 7 & speciesCode == "B")]
  AGB[, biomass := age * 10]
  ids <- pickCompetitionTables(AGB, n = 12L)
  expect_lte(length(ids), 12L)
  expect_false(7L %in% ids) # single-species table
  expect_setequal(unique(AGB[yieldTableIndex %in% ids]$speciesCode), c("A", "B", "C"))
  prev <- data.table::data.table(yieldTableIndex = 1:30, ecoregionGroup = "g", nPixels = 1L)
  expect_s3_class(pltfnCompetition(AGB, prev), "ggplot")
})
