## The yield-table contract (helper-yieldTableContract.R), on small synthetic tables.

ytGood <- function() {
  data.table::data.table(yieldTableIndex = 1L, speciesCode = factor(rep(c("Pice_eng", "Abie_las"), each = 4)),
                         age = rep(0:3, 2), biomass = c(1L, 30L, 90L, 200L, 1L, 25L, 80L, 170L))
}

test_that("a table from age 0 with near-zero biomass and consecutive ages passes", {
  expect_identical(yieldTableViolations(ytGood(), maxAge = 3L), character(0))
  expect_success(expectYieldTableContract(ytGood()))
})

test_that("a table that starts at age 1 breaks it (no age-0 row for CBM)", {
  expect_identical(yieldTableViolations(ytGood()[age > 0]), "no age-0 row")
  expect_failure(expectYieldTableContract(ytGood()[age > 0]), "no age-0 row")
})

test_that("an age-0 row carrying real biomass breaks it", {
  expect_identical(yieldTableViolations(ytGood()[age == 0, biomass := 10L]), "age-0 biomass above 1 g/m2")
})

test_that("repeated age-0 rows (Biomass_core holds age 0 until reclassification) break it unless relabelled", {
  yt <- data.table::data.table(yieldTableIndex = 1L, speciesCode = factor("Pice_eng"),
                               age = c(0L, 0L, 0L, 3L), biomass = c(1L, 30L, 90L, 200L))
  expect_true("repeated ages" %in% yieldTableViolations(yt))
})

test_that("a gap in ages, negative biomass and ages past maxAge break it", {
  expect_identical(yieldTableViolations(ytGood()[age != 2]), "gaps in ages")
  expect_identical(yieldTableViolations(ytGood()[age == 3, biomass := -1L]), "negative biomass")
  expect_identical(yieldTableViolations(ytGood(), maxAge = 2L), "ages beyond maxAge")
  expect_identical(yieldTableViolations(ytGood(), maxAge = 5L), character(0)) # tables may end before maxAge
})
