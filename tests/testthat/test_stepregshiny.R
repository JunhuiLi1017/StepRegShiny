library(testthat)
library(StepRegShiny)

test_that("StepRegShinyApp function exists", {
  expect_true(exists("StepRegShinyApp"))
  expect_true(is.function(StepRegShinyApp))
})

test_that("Package loads without errors", {
  expect_no_error(library(StepRegShiny))
})
