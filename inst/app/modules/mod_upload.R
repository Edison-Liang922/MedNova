.mednova_upload_task_choices <- function() {
  c(
    "请选择任务类型" = "",
    "PSM" = "propensity_score_matching",
    "Logistic" = "logistic_regression",
    "Cox" = "cox_regression",
    "Bulk DEG" = "bulk_rna_differential_expression",
    "MR" = "mendelian_randomization",
    "Diagnostic" = "diagnostic_modeling"
  )
}

.mednova_upload_single_input <- function(ns, id, label, choices) {
  shiny::div(
    class = "mapping-grid__item",
    shiny::selectizeInput(
      inputId = ns(id),
      label = label,
      choices = c("请选择列" = "", choices),
      selected = "",
      multiple = FALSE,
      options = list(placeholder = "请选择列")
    )
  )
}

.mednova_upload_multi_input <- function(ns, id, label, choices) {
  shiny::div(
    class = "mapping-grid__item mapping-grid__item--full",
    shiny::selectizeInput(
      inputId = ns(id),
      label = label,
      choices = choices,
      selected = character(),
      multiple = TRUE,
      options = list(
        placeholder = "请选择一个或多个列",
        plugins = list("remove_button")
      )
    )
  )
}

.mednova_upload_dynamic_inputs <- function(ns, domain, current_columns) {
  if (!nzchar(domain)) {
    return(
      shiny::div(
        class = "mapping-section",
        shiny::tags$h3("当前任务输入"),
        shiny::div(class = "studio-empty studio-empty--small", "请先选择任务类型。")
      )
    )
  }

  if (!length(current_columns)) {
    return(
      shiny::div(
        class = "mapping-section",
        shiny::tags$h3("当前任务输入"),
        shiny::div(class = "studio-empty studio-empty--small", "上传数据后即可配置当前任务输入。")
      )
    )
  }

  input_ui <- switch(
    domain,
    propensity_score_matching = shiny::tagList(
      .mednova_upload_single_input(ns, "treatment", "治疗 / 暴露列", current_columns),
      .mednova_upload_multi_input(ns, "covariates", "协变量 / 预测变量", current_columns)
    ),
    logistic_regression = shiny::tagList(
      .mednova_upload_single_input(ns, "outcome", "结局列", current_columns),
      .mednova_upload_multi_input(ns, "covariates", "协变量 / 预测变量", current_columns)
    ),
    cox_regression = shiny::tagList(
      .mednova_upload_single_input(ns, "time", "时间列", current_columns),
      .mednova_upload_single_input(ns, "event", "事件列", current_columns),
      .mednova_upload_multi_input(ns, "covariates", "协变量 / 预测变量", current_columns)
    ),
    bulk_rna_differential_expression = shiny::tagList(
      .mednova_upload_single_input(ns, "group", "分组列", current_columns),
      .mednova_upload_single_input(ns, "sample_id", "样本 ID 列（可选）", current_columns)
    ),
    mendelian_randomization = shiny::tagList(
      .mednova_upload_single_input(ns, "snp", "SNP 列", current_columns),
      .mednova_upload_single_input(ns, "beta", "beta 列", current_columns),
      .mednova_upload_single_input(ns, "se", "SE 列", current_columns),
      .mednova_upload_single_input(ns, "eaf", "EAF 列", current_columns),
      .mednova_upload_single_input(ns, "effect_allele", "effect allele 列", current_columns),
      .mednova_upload_single_input(ns, "other_allele", "other allele 列", current_columns),
      .mednova_upload_single_input(ns, "pval", "P 值列", current_columns),
      .mednova_upload_single_input(ns, "n", "样本量列", current_columns),
      .mednova_upload_single_input(ns, "chr", "染色体列", current_columns),
      .mednova_upload_single_input(ns, "pos", "位置列", current_columns),
      shiny::div(
        class = "mapping-grid__item mapping-grid__item--full",
        shiny::textInput(
          inputId = ns("outcome_data_name"),
          label = "Outcome 数据对象名",
          value = "outcome_data",
          placeholder = "outcome_data"
        )
      ),
      shiny::div(
        class = "mapping-grid__item mapping-grid__item--full",
        shiny::textInput(
          inputId = ns("exposure_name"),
          label = "Exposure 名称",
          value = "Exposure",
          placeholder = "Exposure"
        )
      )
    ),
    diagnostic_modeling = shiny::tagList(
      .mednova_upload_single_input(ns, "outcome", "结局 / 分组列", current_columns),
      .mednova_upload_multi_input(ns, "covariates", "预测变量", current_columns),
      .mednova_upload_single_input(ns, "sample_id", "样本 ID 列（可选）", current_columns)
    ),
    shiny::div(class = "studio-empty studio-empty--small", "暂不支持该任务类型。")
  )

  shiny::div(
    class = "mapping-section",
    shiny::div(
      class = "mapping-section__intro",
      shiny::tags$h3("当前任务输入"),
      shiny::tags$p("只显示当前任务需要配置的输入项。")
    ),
    shiny::div(class = "mapping-grid", input_ui)
  )
}

mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "studio-card",
    shiny::tags$h2("分析输入配置"),
    shiny::fileInput(
      inputId = ns("file"),
      label = "上传数据文件",
      accept = c(".csv", ".tsv", ".xlsx", ".rds")
    ),
    shiny::selectInput(
      inputId = ns("task_type"),
      label = "任务类型",
      choices = .mednova_upload_task_choices(),
      selected = ""
    ),
    shiny::textAreaInput(
      inputId = ns("task_note"),
      label = "任务备注（可选）",
      placeholder = paste(
        "可填写补充信息，例如：",
        "- 输出 odds ratio",
        "- 需要 love plot",
        "- 备注当前结局定义",
        sep = "\n"
      ),
      rows = 4
    ),
    shiny::textInput(
      inputId = ns("data_name"),
      label = "数据对象名",
      value = "input_data",
      placeholder = "input_data"
    ),
    shiny::uiOutput(ns("task_inputs")),
    shiny::div(
      class = "studio-actions",
      shiny::actionButton(ns("generate"), "生成代码", class = "btn-primary"),
      shiny::actionButton(ns("reset"), "重置", class = "btn-default")
    )
  )
}

mod_upload_server <- function(id, column_names = NULL) {
  if (is.null(column_names)) {
    column_names <- shiny::reactive(character())
  }

  shiny::moduleServer(id, function(input, output, session) {
    uploaded_file <- shiny::reactiveVal(NULL)

    output$task_inputs <- shiny::renderUI({
      .mednova_upload_dynamic_inputs(
        ns = session$ns,
        domain = trimws(input$task_type %||% ""),
        current_columns = unique(column_names() %||% character())
      )
    })

    shiny::observeEvent(input$file, {
      uploaded_file(input$file)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$reset, {
      uploaded_file(NULL)
      shiny::updateSelectInput(session, "task_type", selected = "")
      shiny::updateTextAreaInput(session, "task_note", value = "")
      shiny::updateTextInput(session, "data_name", value = "input_data")
      session$sendCustomMessage("mednova-clear-file", list(id = session$ns("file")))
    }, ignoreInit = TRUE)

    single_value <- function(value, current_columns) {
      if (!is.character(value) || length(value) != 1L || !nzchar(trimws(value))) {
        return(NULL)
      }

      if (!value %in% current_columns) {
        return(NULL)
      }

      value
    }

    multi_value <- function(value, current_columns) {
      if (is.null(value) || !length(value)) {
        return(character())
      }

      unique(as.character(value)[as.character(value) %in% current_columns])
    }

    list(
      file = shiny::reactive(uploaded_file()),
      task_note = shiny::reactive(trimws(input$task_note %||% "")),
      task_type = shiny::reactive(trimws(input$task_type %||% "")),
      task_domain = shiny::reactive(trimws(input$task_type %||% "")),
      data_name = shiny::reactive({
        value <- trimws(input$data_name %||% "")
        if (nzchar(value)) value else "input_data"
      }),
      input_values = shiny::reactive({
        current_columns <- unique(column_names() %||% character())

        list(
          treatment = single_value(input$treatment, current_columns),
          outcome = single_value(input$outcome, current_columns),
          time = single_value(input$time, current_columns),
          event = single_value(input$event, current_columns),
          sample_id = single_value(input$sample_id, current_columns),
          group = single_value(input$group, current_columns),
          covariates = multi_value(input$covariates, current_columns),
          snp = single_value(input$snp, current_columns),
          beta = single_value(input$beta, current_columns),
          se = single_value(input$se, current_columns),
          eaf = single_value(input$eaf, current_columns),
          effect_allele = single_value(input$effect_allele, current_columns),
          other_allele = single_value(input$other_allele, current_columns),
          pval = single_value(input$pval, current_columns),
          n = single_value(input$n, current_columns),
          chr = single_value(input$chr, current_columns),
          pos = single_value(input$pos, current_columns),
          outcome_data_name = trimws(input$outcome_data_name %||% ""),
          exposure_name = trimws(input$exposure_name %||% "")
        )
      }),
      generate = shiny::reactive(input$generate),
      reset = shiny::reactive(input$reset)
    )
  })
}
