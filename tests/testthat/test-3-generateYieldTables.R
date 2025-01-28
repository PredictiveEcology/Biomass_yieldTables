test_that("function generateYieldTables works", {
  
  ngroup <- 3
  age <- c(rep(0, 11), c(11:20))
  nsp <- 2
  
  cohortDataAll <- data.table::as.data.table(
    expand.grid(pixelGroup = c(1:ngroup),
                age = age,
                speciesCode = as.factor(c(1:nsp)))
  )
  
  cohortDataAll$B <- as.integer(round(runif(nrow(cohortDataAll), min = 1, max = 100)))
  
  out <- generateYieldTables(cohortDataAll)
  
  # inspect out
  expect_no_error(generateYieldTables(cohortDataAll))
  expect_is(out, "list")
  expect_true(length(out) == 2)
  expect_true(all(names(out) == c("cds", "cdSpeciesCodes")))

  # inspect cds
  expect_is(out$cds, "data.table")
  expect_true(all(colnames(out$cds) %in% c("pixelGroup", "age", "B", "cohort_id")))
  expect_equal(sum(out$cds$age == 0), nsp*ngroup)
  expect_equal(nrow(out$cds), ngroup*(max(age)+1)*nsp)
  
  # inspect cdSpeciesCodes
  expect_true(all(colnames(out$cdSpeciesCodes) %in% c("cohort_id", "pixelGroup", "speciesCode")))
  expect_false(any(duplicated(out$cdSpeciesCodes$cohort_id)))
  expect_equal(nrow(out$cdSpeciesCodes), nsp*ngroup)
              
})
