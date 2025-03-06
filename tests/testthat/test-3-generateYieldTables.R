test_that("function generateYieldTables works", {
  ngroup <- 6
  age <- c(rep(0, 11), c(11:20))
  nsp <- 2
  
  cohortDataAll <- data.table::as.data.table(
    expand.grid(yieldPixelGroup = c(1:(ngroup)),
                age = age,
                speciesCode = as.factor(c(1:nsp)))
  )
  
  cohortDataAll$B <- as.integer(round(runif(nrow(cohortDataAll), min = 1, max = 100)))
  
  
  out <- generateYieldTables(cohortDataAll)
  # inspect out
  expect_no_error(generateYieldTables(cohortDataAll))
  expect_is(out, "list")
  expect_named(out, c("cds", "cdSpeciesCodes"))

  # inspect cds
  expect_is(out$cds, "data.table")
  expect_named(out$cds, c("yieldPixelGroup", "age", "B", "cohort_id"))
  expect_equal(sum(out$cds$age == 0), nsp*ngroup)
  expect_equal(nrow(out$cds), ngroup*(max(age)+1)*nsp)
  
  # inspect cdSpeciesCodes
  expect_named(out$cdSpeciesCodes, c("cohort_id", "yieldPixelGroup", "speciesCode"))
  expect_false(any(duplicated(out$cdSpeciesCodes$cohort_id)))
  expect_equal(nrow(out$cdSpeciesCodes), nsp*ngroup)
  
  expect_setequal(out$cds$cohort_id, out$cdSpeciesCodes$cohort_id)
  expect_setequal(out$cds$yieldPixelGroup, out$cdSpeciesCodes$yieldPixelGroup)
  
})
