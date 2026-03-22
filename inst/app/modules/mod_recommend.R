mod_recommend_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "info-section",
    shiny::div(
      class = "info-section__intro",
      shiny::tags$h3("函数处理流程"),
      shiny::tags$p("当前主函数和处理步骤。")
    ),
    shiny::uiOutput(ns("summary")),
    shiny::uiOutput(ns("steps"))
  )
}

mod_recommend_server <- function(id, process_info, error) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-empty", "请先修正输入错误。"))
      }

      info <- process_info()
      if (is.null(info)) {
        return(shiny::div(class = "studio-empty", "请选择任务类型。"))
      }

      shiny::tagList(
        shiny::div(
          class = "domain-block",
          shiny::tags$span(class = "domain-block__label", "当前任务"),
          shiny::tags$span(class = "domain-badge", info$task_label_cn)
        ),
        shiny::div(
          class = "domain-block",
          shiny::tags$span(class = "domain-block__label", "当前主函数"),
          shiny::tags$span(class = "domain-badge", info$main_function)
        )
      )
    })

    output$steps <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-empty", "请先修正输入错误。"))
      }

      info <- process_info()
      if (is.null(info) || !length(info$steps)) {
        return(shiny::div(class = "studio-empty", "请选择任务类型后查看处理流程。"))
      }

      shiny::tagList(
        shiny::tags$h4("处理流程"),
        shiny::tags$ol(
          class = "workflow-list",
          lapply(info$steps, shiny::tags$li)
        )
      )
    })
  })
}
