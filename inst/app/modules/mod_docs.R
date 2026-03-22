.mednova_docs_group_card <- function(title, description, functions) {
  shiny::div(
    class = "platform-doc-card",
    shiny::tags$h3(title),
    shiny::tags$p(description),
    shiny::div(
      class = "platform-chip-row",
      lapply(functions, function(x) shiny::tags$span(class = "chip", x))
    )
  )
}

mod_docs_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "platform-page platform-page--docs",
    shiny::div(
      class = "platform-page-header",
      shiny::tags$span(class = "platform-kicker", "文档中心"),
      shiny::tags$h2("主接口、数据要求与工作台说明"),
      shiny::tags$p(
        class = "platform-lead",
        "平台入口、统一分析接口、任务输入和 Studio 的使用方式都集中在这一处。"
      ),
      shiny::div(
        class = "platform-button-row",
        shiny::actionButton(ns("go_studio"), "进入 Studio", class = "btn-primary"),
        shiny::actionButton(ns("go_cases"), "浏览教程案例", class = "btn-default")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台简介"),
        shiny::tags$p("MedNova 将任务配置、统一入口函数和 R 脚本生成放在同一个平台中。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_docs_group_card(
          "平台启动入口",
          "启动统一平台 app，在同一个应用中访问首页、平台介绍、Studio、文档中心、案例和 FAQ。",
          c("run_mednova_app")
        ),
        .mednova_docs_group_card(
          "任务与脚本入口",
          "在 R 会话中直接生成任务结果对象或脚本字符串。",
          c("mednova_task_assistant", "mednova_generate_script")
        ),
        .mednova_docs_group_card(
          "引擎函数",
          "提供结构检查、任务匹配和流程规划，适合高级调用或二次封装。",
          c("mednova_inspect_data", "mednova_match_task", "mednova_plan_workflow")
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("统一分析入口"),
        shiny::tags$p("对外优先使用 `med_*` 统一入口函数，不再以旧函数链作为默认推荐接口。")
      ),
      shiny::div(
        class = "platform-doc-grid",
        .mednova_docs_group_card(
          "倾向评分匹配",
          "匹配、baseline table、balance table 和 love plot 输出统一收口到一个入口。",
          c("med_psm")
        ),
        .mednova_docs_group_card(
          "回归分析",
          "Logistic 与 Cox 分别提供统一入口，便于直接生成脚本和结果对象。",
          c("med_logistic", "med_cox")
        ),
        .mednova_docs_group_card(
          "表达、MR 与诊断模型",
          "Bulk 差异表达、MR 和诊断模型都优先通过统一入口调用。",
          c("med_bulk_deg", "med_mr", "med_diagnostic_model")
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("安装与启动"),
        shiny::tags$p("本地开发环境可直接加载项目并启动平台入口。")
      ),
      shiny::tags$pre(
        class = "platform-code-block",
        shiny::tags$code(
"devtools::load_all()
run_mednova_app()

psm_res <- med_psm(
  data = analysis_data,
  treat_col = \"treatment\",
  covariates = c(\"age\", \"sex\", \"bmi\"),
  return_baseline = TRUE,
  return_love_plot = TRUE
)"
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("数据要求"),
        shiny::tags$p("不同任务需要的最低输入不同，平台会围绕任务需求组织输入项。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_docs_group_card(
          "PSM / Logistic / Cox / 诊断模型",
          "通常需要临床表或样本级表格，并明确治疗列、结局列、时间列、事件列或预测变量。",
          c("treatment", "outcome", "time", "event", "covariates")
        ),
        .mednova_docs_group_card(
          "Bulk RNA 差异表达",
          "通常需要表达矩阵与样本分组信息，可按需要补充样本 ID。",
          c("group", "sample_id")
        ),
        .mednova_docs_group_card(
          "孟德尔随机化",
          "通常需要 GWAS 汇总统计字段，如 SNP、beta、SE、P 值和等位基因信息。",
          c("snp", "beta", "se", "pval", "effect_allele", "other_allele", "n")
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("当前任务输入"),
        shiny::tags$p("Studio 中的输入项由任务类型决定，只显示当前任务真正需要配置的字段。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_docs_group_card(
          "PSM",
          "选择治疗 / 暴露列和协变量 / 预测变量。",
          c("treatment", "covariates")
        ),
        .mednova_docs_group_card(
          "Logistic / Cox",
          "Logistic 需要结局列和预测变量；Cox 需要时间列、事件列和预测变量。",
          c("outcome", "time", "event", "covariates")
        ),
        .mednova_docs_group_card(
          "Bulk / MR / Diagnostic",
          "Bulk 关注分组列；MR 关注 GWAS 关键字段；诊断模型关注结局列和预测变量。",
          c("group", "sample_id", "snp", "beta", "outcome", "covariates")
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("App 使用说明"),
        shiny::tags$p("Studio 工作台围绕任务配置和代码生成展开。")
      ),
      shiny::tags$ol(
        class = "platform-step-list",
        shiny::tags$li("上传数据文件。"),
        shiny::tags$li("选择任务类型。"),
        shiny::tags$li("填写任务备注和数据对象名。"),
        shiny::tags$li("配置当前任务输入。"),
        shiny::tags$li("查看输入数据信息、函数处理流程和函数输入要求。"),
        shiny::tags$li("生成、复制或下载 `.R` 脚本。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("兼容函数"),
        shiny::tags$p("旧函数继续保留以保证兼容，但不再作为文档和工作台的主推入口。")
      ),
      shiny::div(
        class = "platform-note-card",
        shiny::div(
          class = "platform-chip-row",
          shiny::tags$span(class = "chip", "Epi_PSM_match"),
          shiny::tags$span(class = "chip", "Epi_PSM_baseline_tables"),
          shiny::tags$span(class = "chip", "Epi_logistic_univ"),
          shiny::tags$span(class = "chip", "Epi_logistic_multiv"),
          shiny::tags$span(class = "chip", "Epi_cox_multiv"),
          shiny::tags$span(class = "chip", "Bio_Bulk_align_group"),
          shiny::tags$span(class = "chip", "Bio_Bulk_filter_low_expression"),
          shiny::tags$span(class = "chip", "Bio_Bulk_limma_analysis"),
          shiny::tags$span(class = "chip", "Bio_MR_process_exposure"),
          shiny::tags$span(class = "chip", "Bio_MR_prepare_outcome"),
          shiny::tags$span(class = "chip", "Bio_MR_pipeline"),
          shiny::tags$span(class = "chip", "Bio_build_diagnostic_model")
        ),
        shiny::tags$p("这些函数仍可用于兼容和内部调用，但页面展示、案例说明和脚本生成优先使用 `med_*` 统一入口。")
      )
    )
  )
}

mod_docs_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      go_studio = shiny::reactive(input$go_studio),
      go_cases = shiny::reactive(input$go_cases)
    )
  })
}
