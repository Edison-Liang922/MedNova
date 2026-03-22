.mednova_app_value_box <- function(label, value) {
  shiny::div(
    class = "profile-item",
    shiny::tags$span(class = "profile-item__label", label),
    shiny::tags$span(class = "profile-item__value", value)
  )
}

.mednova_app_chip_block <- function(title, values) {
  shiny::div(
    class = "profile-block",
    shiny::tags$h4(title),
    if (!length(values)) {
      shiny::div(class = "studio-empty studio-empty--small", "暂无。")
    } else {
      shiny::div(
        class = "chip-wrap",
        lapply(values, function(value) shiny::tags$span(class = "chip", value))
      )
    }
  )
}

.mednova_app_preview_data <- function(data, n = 6L) {
  if (is.null(data)) {
    return(data.frame())
  }

  if (is.data.frame(data)) {
    return(utils::head(data, n))
  }

  if (is.matrix(data)) {
    return(utils::head(as.data.frame(data, check.names = FALSE), n))
  }

  if (is.list(data)) {
    component_names <- names(data) %||% paste0("component_", seq_along(data))
    return(data.frame(
      component = component_names,
      class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1L)),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(value = utils::head(as.character(data), n), stringsAsFactors = FALSE)
}

mod_profile_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "info-section",
    shiny::div(
      class = "info-section__intro",
      shiny::tags$h3("输入数据信息"),
      shiny::tags$p("行数、列数、列名和数据预览。")
    ),
    shiny::uiOutput(ns("summary")),
    shiny::uiOutput(ns("column_blocks")),
    shiny::div(
      class = "profile-block",
      shiny::tags$h4("数据预览"),
      DT::DTOutput(ns("preview"))
    )
  )
}

mod_profile_server <- function(id, profile, preview_data, error) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-alert", error()))
      }

      profile_value <- profile()
      if (is.null(profile_value)) {
        return(shiny::div(class = "studio-empty", "上传数据后会显示基础信息。"))
      }

      dims <- profile_value$dimensions %||% list(rows = NA_integer_, cols = NA_integer_)

      shiny::div(
        class = "profile-grid",
        .mednova_app_value_box("对象类型", paste(profile_value$object_class, collapse = ", ")),
        .mednova_app_value_box("对象类别", profile_value$object_type %||% "unknown"),
        .mednova_app_value_box("行数", dims$rows %||% NA_integer_),
        .mednova_app_value_box("列数", dims$cols %||% NA_integer_),
        .mednova_app_value_box("对象名称", profile_value$object_name %||% "input_data")
      )
    })

    output$column_blocks <- shiny::renderUI({
      if (!is.null(error())) {
        return(shiny::div(class = "studio-empty", "请先修正输入错误。"))
      }

      profile_value <- profile()
      if (is.null(profile_value)) {
        return(shiny::div(class = "studio-empty", "上传数据后会显示列信息。"))
      }

      shiny::tagList(
        .mednova_app_chip_block("列名", profile_value$column_names %||% character()),
        .mednova_app_chip_block("数值列", profile_value$numeric_columns %||% character()),
        .mednova_app_chip_block("字符列", profile_value$character_columns %||% character())
      )
    })

    output$preview <- DT::renderDT({
      if (!is.null(error())) {
        return(DT::datatable(
          data.frame(提示 = error(), stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
        ))
      }

      preview_value <- .mednova_app_preview_data(preview_data())
      if (!nrow(preview_value)) {
        return(DT::datatable(
          data.frame(提示 = "上传数据后会显示前几行。", stringsAsFactors = FALSE),
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
        ))
      }

      DT::datatable(
        preview_value,
        rownames = FALSE,
        options = list(
          pageLength = 6,
          lengthChange = FALSE,
          scrollX = TRUE
        )
      )
    })
  })
}
