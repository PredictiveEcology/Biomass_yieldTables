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
  
  # general
  expect_true(inherits(pltfn(AGB, 3), what = "ggplot"))
  expect_error(pltfn())
  expect_error(pltfn(AGB, 0))
  expect_invisible(pltfn(AGB, 2))
  expect_message(pltfn(AGB, 10))
  expect_no_error(pltfn(AGB, 1))
  
  # number of facets
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 2))$data[[1]]$PANEL)), 2)
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, 10))$data[[1]]$PANEL)), ngroup)
})

test_that("pltfn uses the supplied species colours", {
  AGB <- data.table::data.table(yieldTableIndex = 1L, age = 1:3, speciesCode = "Pice_eng", biomass = 1:3)
  gg <- pltfn(AGB, 1, colours = c(Pice_eng = "#112233"))
  expect_equal(unique(ggplot2::ggplot_build(gg)$data[[1]]$colour), "#112233")
})
