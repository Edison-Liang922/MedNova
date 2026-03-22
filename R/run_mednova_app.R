#' 启动 MedNova 平台
#'
#' 启动统一的 MedNova Shiny 平台入口，在同一个应用中访问平台首页、
#' 平台介绍、Studio 工作台、函数与文档、教程案例和 FAQ。
#'
#' @param launch.browser 是否在浏览器中打开应用。
#' @param host 传递给 [shiny::runApp()] 的 host。
#' @param port 传递给 [shiny::runApp()] 的 port。为 `NULL` 时由 Shiny 自行处理。
#' @param display.mode 传递给 [shiny::runApp()] 的显示模式。
#' @param return_app 逻辑值；当 `TRUE` 时返回 Shiny app 对象而不直接启动。
#'   这个参数主要用于 smoke test。
#'
#' @return 不显式返回时会启动应用；当 `return_app = TRUE` 时返回
#'   一个 `shiny.appobj`。
#' @export
run_mednova_app <- function(launch.browser = interactive(),
                            host = "127.0.0.1",
                            port = NULL,
                            display.mode = c("auto", "normal", "showcase"),
                            return_app = FALSE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("运行 MedNova Studio 需要安装 `shiny` 包。", call. = FALSE)
  }
  if (!requireNamespace("bslib", quietly = TRUE)) {
    stop("运行 MedNova Studio 需要安装 `bslib` 包。", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("运行 MedNova Studio 需要安装 `DT` 包。", call. = FALSE)
  }

  display.mode <- match.arg(display.mode)
  app_dir <- .mednova_resource_path("app")

  if (isTRUE(return_app)) {
    return(shiny::shinyAppDir(appDir = app_dir))
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    host = host,
    port = port,
    display.mode = display.mode
  )
}
