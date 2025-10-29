library(testthat)
library(ggplot2)

test_that("createPlot returns list of ggplot objects for univariate plot types", {
  createPlot <- getFromNamespace("createPlot", "StepRegShiny")

  df <- data.frame(
    x = rnorm(30),
    y = rnorm(30),
    z = rnorm(30)
  )

  vars <- c("x", "y")

  for (ptype in c("Bar plot", "Box plot", "Density plot", "Dot plot", "Histogram", "QQ plot")) {
    res <- createPlot(ptype, vars, df)
    expect_type(res, "list")
    expect_length(res, length(vars))
    lapply(res, function(p) expect_s3_class(p, "gg"))
  }
})

test_that("createPlot returns correlation plot as a single ggplot in a list", {
  createPlot <- getFromNamespace("createPlot", "StepRegShiny")

  df <- data.frame(
    x = rnorm(30),
    y = rnorm(30),
    z = rnorm(30)
  )

  res <- createPlot("Correlation plot", c("x", "y", "z"), df)
  expect_type(res, "list")
  expect_length(res, 1L)
  expect_s3_class(res[[1]], "gg")
})

test_that("createPlot returns list of ggplot objects for Scatter and Line plot", {
  createPlot <- getFromNamespace("createPlot", "StepRegShiny")

  df <- data.frame(
    a = rnorm(20),
    b = rnorm(20),
    c = rnorm(20)
  )

  # use one variable as y, rest as x facets
  res <- createPlot("Scatter and Line plot", c("a", "b"), df)
  expect_type(res, "list")
  expect_length(res, 2L)
  lapply(res, function(p) expect_s3_class(p, "gg"))
})

test_that("createPlot returns NULL for unknown plot type", {
  createPlot <- getFromNamespace("createPlot", "StepRegShiny")

  df <- data.frame(x = rnorm(10))
  res <- createPlot("Unknown plot", c("x"), df)
  expect_null(res)
})


