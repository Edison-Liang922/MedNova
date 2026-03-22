mod_code_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "studio-card studio-card--code",
    shiny::div(
      class = "code-toolbar",
      shiny::div(
        class = "code-toolbar__title",
        shiny::tags$h2("生成的 R 脚本"),
        shiny::tags$p("可直接带入项目的 R 脚本")
      ),
      shiny::div(
        class = "code-toolbar__actions",
        shiny::actionButton(ns("copy"), "复制"),
        shiny::downloadButton(ns("download"), "下载 .R 文件")
      )
    ),
    shiny::uiOutput(ns("status")),
    shiny::div(
      class = "code-editor-shell",
      shiny::div(
        class = "code-editor-shell__bar",
        shiny::div(
          class = "code-editor-shell__dots",
          shiny::tags$span(class = "dot dot--red"),
          shiny::tags$span(class = "dot dot--yellow"),
          shiny::tags$span(class = "dot dot--green")
        ),
        shiny::tags$span(class = "code-editor-shell__name", "mednova_script.R")
      ),
      shiny::div(
        class = "code-scroll",
        shiny::verbatimTextOutput(ns("script"), placeholder = TRUE)
      )
    )
  )
}

mod_code_server <- function(id, result, error) {
  shiny::moduleServer(id, function(input, output, session) {
    script_text <- shiny::reactive({
      res <- result()
      if (is.null(res)) {
        return("")
      }

      res$script %||% ""
    })

    output$status <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-alert", error()))
      }

      if (is.null(result())) {
        return(shiny::div(class = "studio-empty", "生成结果后会显示 R 脚本。"))
      }

      NULL
    })

    output$script <- shiny::renderText({
      script_text()
    })

    shiny::observeEvent(input$copy, {
      req(result())
      session$sendCustomMessage(
        "mednova-copy-script",
        list(text = script_text())
      )
      shiny::showNotification("脚本已复制到剪贴板。", type = "message", duration = 2)
    }, ignoreInit = TRUE)

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("mednova_script_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(script_text(), con = file, useBytes = TRUE)
      }
    )
  })
}
