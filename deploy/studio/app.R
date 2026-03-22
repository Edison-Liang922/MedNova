suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(readxl))

detect_app_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      ofile <- frame$ofile
      if (is.null(ofile) || !length(ofile)) "" else as.character(ofile[[1L]])
    },
    character(1L)
  )
  frame_files <- frame_files[nzchar(frame_files)]

  if (length(frame_files)) {
    return(dirname(normalizePath(tail(frame_files, 1L), winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

app_dir <- detect_app_dir()

ensure_mednova_available <- function() {
  project_root <- normalizePath(
    file.path(app_dir, "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )

  if (file.exists(file.path(project_root, "DESCRIPTION")) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(
      path = project_root,
      export_all = FALSE,
      helpers = FALSE,
      quiet = TRUE
    )
    return(invisible(TRUE))
  }

  if (requireNamespace("MedNova", quietly = TRUE)) {
    suppressPackageStartupMessages(library(MedNova))
    return(invisible(TRUE))
  }

  stop(
    paste(
      "无法加载 MedNova 包。",
      "请先安装 MedNova，或在本地开发环境中安装 `pkgload` 后再运行 `shiny::runApp(\"deploy/studio\")`。",
      sep = "\n"
    ),
    call. = FALSE
  )
}

ensure_mednova_available()

app <- MedNova:::.mednova_studio_app()
app
