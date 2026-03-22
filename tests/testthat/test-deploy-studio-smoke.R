testthat::test_that("studio deployment app factory returns a shiny app object", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")

  if (!"MedNova" %in% loadedNamespaces()) {
    testthat::skip_if_not_installed("pkgload")
    pkgload::load_all(testthat::test_path("..", ".."), quiet = TRUE)
  }

  testthat::expect_true(exists(".mednova_studio_app", envir = asNamespace("MedNova"), inherits = FALSE))

  app <- suppressWarnings(MedNova:::.mednova_studio_app())

  testthat::expect_s3_class(app, "shiny.appobj")
})

testthat::test_that("studio deployment entry exists", {
  deploy_app <- testthat::test_path("..", "..", "deploy", "studio", "app.R")

  testthat::expect_true(file.exists(deploy_app))
})
