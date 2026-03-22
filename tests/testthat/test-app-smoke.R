testthat::test_that("run_mednova_app can return a shiny app object", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("DT")

  app <- suppressWarnings(run_mednova_app(return_app = TRUE))

  testthat::expect_s3_class(app, "shiny.appobj")
})
