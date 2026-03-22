mod_requirements_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "info-section",
    shiny::div(
      class = "info-section__intro",
      shiny::tags$h3("函数输入要求"),
      shiny::tags$p("核对当前主函数所需输入。")
    ),
    shiny::uiOutput(ns("summary")),
    DT::DTOutput(ns("requirements"))
  )
}

mod_requirements_server <- function(id, requirements, error) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-alert", error()))
      }

      requirement_obj <- requirements()
      if (is.null(requirement_obj)) {
        return(shiny::div(class = "studio-empty", "请选择任务类型后查看输入要求。"))
      }

      status_class <- if (isTRUE(requirement_obj$is_complete)) {
        "domain-badge"
      } else {
        "chip"
      }

      shiny::tagList(
        shiny::div(
          class = "domain-block",
          shiny::tags$span(class = "domain-block__label", "当前主函数"),
          shiny::tags$span(class = "domain-badge", requirement_obj$main_function)
        ),
        shiny::div(
          class = "domain-block",
          shiny::tags$span(class = "domain-block__label", "状态"),
          shiny::tags$span(class = status_class, requirement_obj$status_text)
        )
      )
    })

    output$requirements <- DT::renderDT({
      if (!is.null(error())) {
        return(DT::datatable(
          data.frame(提示 = error(), stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
        ))
      }

      requirement_obj <- requirements()
      if (is.null(requirement_obj) || !is.data.frame(requirement_obj$table) || !nrow(requirement_obj$table)) {
        return(DT::datatable(
          data.frame(提示 = "请选择任务类型后查看输入要求。", stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
        ))
      }

      DT::datatable(
        requirement_obj$table,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE
        )
      )
    })
  })
}
