suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(pkgload))

pkgload::load_all(
  path = ".",
  export_all = FALSE,
  helpers = FALSE,
  quiet = TRUE
)

app <- MedNova:::.mednova_studio_app()
app
