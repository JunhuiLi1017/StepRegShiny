library(testthat)
library(StepRegShiny)

test_that("StepRegGUI function exists", {
  expect_true(exists("StepRegGUI"))
  expect_true(is.function(StepRegGUI))
})

test_that("Package loads without errors", {
  expect_no_error(library(StepRegShiny))
})
