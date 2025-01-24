test_that("plot function works", {
  
  library(data.table)
  library(ggplot2)
  
  #####
  # dummy inputs
  ngroup <- 3
  nage <- 5
  nsp <- 2
  AGB <- data.table::as.data.table(
    expand.grid(pixelGroup = c(1:ngroup),
                age = c(1:nage),
                speciesCode = as.factor(c(1:nsp)))
  )
  AGB$B <- AGB$age
  # Add cohort_id. One cohort_id per pixelGroup x species
  AGB[, cohort_id:=.GRP, by = c("pixelGroup", "speciesCode")]
  # Create reference table
  sp <- unique(AGB[, .(cohort_id, pixelGroup, speciesCode)])
  # Remove columns
  AGB[, speciesCode := NULL]
  #####
  
  # general
  expect_true(inherits(pltfn(AGB, sp, 3), what = "ggplot"))
  expect_error(pltfn())
  expect_error(pltfn(AGB, sp, 0))
  expect_invisible(pltfn(AGB, sp, 2))
  expect_message(pltfn(AGB, sp, 10))
  expect_no_error(pltfn(AGB, sp, 1))
  
  # number of facets
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, sp, 2))$data[[1]]$PANEL)), 2)
  expect_equal(length(levels(ggplot2::ggplot_build(pltfn(AGB, sp, 10))$data[[1]]$PANEL)), ngroup)
  
})
