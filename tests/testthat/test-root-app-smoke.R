testthat::test_that("repository root app entry returns a shiny app object", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("pkgload")

  root_app <- testthat::test_path("..", "..", "app.R")
  testthat::expect_true(file.exists(root_app))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(testthat::test_path("..", ".."))

  app <- suppressWarnings(source("app.R", local = new.env())$value)
  testthat::expect_s3_class(app, "shiny.appobj")
})
