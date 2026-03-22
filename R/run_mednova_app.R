#' 启动 MedNova Studio
#'
#' 启动 MedNova Studio 工作台，用于配置任务输入并生成可执行的 R 脚本。
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
  .mednova_require_app_packages()

  display.mode <- match.arg(display.mode)
  app <- .mednova_studio_app()

  if (isTRUE(return_app)) {
    return(app)
  }

  shiny::runApp(
    app,
    launch.browser = launch.browser,
    host = host,
    port = port,
    display.mode = display.mode
  )
}
