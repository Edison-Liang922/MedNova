.mednova_cases_card <- function(title, intro, data_points, studio_points, code_points, caution_points) {
  shiny::div(
    class = "platform-case-card",
    shiny::tags$h3(title),
    shiny::tags$p(class = "platform-case-card__intro", intro),
    shiny::tags$h4("输入数据"),
    shiny::tags$ul(class = "platform-mini-list", lapply(data_points, shiny::tags$li)),
    shiny::tags$h4("在 Studio 中怎么填"),
    shiny::tags$ul(class = "platform-mini-list", lapply(studio_points, shiny::tags$li)),
    shiny::tags$h4("生成的代码类型"),
    shiny::tags$ul(class = "platform-mini-list", lapply(code_points, shiny::tags$li)),
    shiny::tags$h4("常见注意事项"),
    shiny::tags$ul(class = "platform-mini-list", lapply(caution_points, shiny::tags$li))
  )
}

mod_cases_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "platform-page platform-page--cases",
    shiny::div(
      class = "platform-page-header",
      shiny::tags$span(class = "platform-kicker", "教程案例"),
      shiny::tags$h2("通过典型场景理解 MedNova 的输入方式与输出结果"),
      shiny::tags$p(
        class = "platform-lead",
        "常见任务的输入形态、Studio 填写方式和脚本输出类型都集中在这里。"
      ),
      shiny::div(
        class = "platform-button-row",
        shiny::actionButton(ns("go_studio"), "进入 Studio 工作台", class = "btn-primary"),
        shiny::actionButton(ns("go_docs"), "查看文档中心", class = "btn-default")
      )
    ),
    shiny::div(
      class = "platform-case-grid",
      .mednova_cases_card(
        "PSM 案例",
        "适合 treatment / exposure 明确、希望做匹配与平衡检查的临床表场景。",
        data_points = c(
          "患者级临床表。",
          "至少包含 treatment / exposure 列。",
          "至少包含年龄、性别、BMI、合并症等治疗前协变量。"
        ),
        studio_points = c(
          "任务类型选择 PSM。",
          "可在任务备注中补充 love plot 等输出要求。",
          "把 treatment 映射到治疗 / 暴露列。",
          "把临床变量映射到协变量 / 预测变量。"
        ),
        code_points = c(
          "优先生成 med_psm() 脚本。",
          "脚本可同时包含 matched data、baseline table、balance table 和 love plot 输出。"
        ),
        caution_points = c(
          "不要把结局列误放进协变量。",
          "协变量尽量使用治疗前信息。"
        )
      ),
      .mednova_cases_card(
        "Logistic 案例",
        "适合二分类结局建模与 OR 输出需求。",
        data_points = c(
          "一张临床表。",
          "至少包含二分类结局列。",
          "还需要若干候选预测变量。"
        ),
        studio_points = c(
          "任务类型选择 Logistic。",
          "可在任务备注中补充 odds ratio 输出要求。",
          "把 outcome 映射到结局列。",
          "把候选变量映射到协变量 / 预测变量。"
        ),
        code_points = c(
          "优先生成 med_logistic() 脚本。",
          "可包含单因素、多因素和 OR 风格结果输出。"
        ),
        caution_points = c(
          "结局列应能清晰解释为二分类。",
          "不要把 ID 列或纯标签列当作预测变量。"
        )
      ),
      .mednova_cases_card(
        "Cox 案例",
        "适合时间到事件分析和 HR 导向结果解释。",
        data_points = c(
          "一张生存分析用临床表。",
          "包含时间列与事件列。",
          "包含候选协变量。"
        ),
        studio_points = c(
          "任务类型选择 Cox。",
          "可在任务备注中补充 overall survival 等说明。",
          "时间列映射到 time，事件列映射到 event。",
          "临床变量映射到 covariates。"
        ),
        code_points = c(
          "优先生成 med_cox() 脚本。",
          "脚本围绕多因素 Cox 模型和风险比结果展开。"
        ),
        caution_points = c(
          "时间列与事件列不要混淆。",
          "事件列最好明确为 0/1 或两类状态。"
        )
      ),
      .mednova_cases_card(
        "MR 案例",
        "适合 exposure / outcome GWAS summary 已经整理好的场景。",
        data_points = c(
          "暴露与结局的 GWAS 汇总统计。",
          "常见字段包括 snp、beta、se、pval 和等位基因相关列。"
        ),
        studio_points = c(
          "任务类型选择 MR。",
          "按页面要求逐项选择 GWAS 关键列。",
          "建议在上传前先整理好结构化 exposure / outcome 数据。"
        ),
        code_points = c(
          "优先生成 med_mr() 脚本。",
          "脚本会保留 exposure / outcome 列映射和 MR 主流程调用。"
        ),
        caution_points = c(
          "如果上传的是普通临床表，平台会提示结构不匹配。",
          "MR 所需字段通常比普通 clinical table 更多。"
        )
      ),
      .mednova_cases_card(
        "诊断模型案例",
        "适合二分类诊断标签与候选特征组合的建模场景。",
        data_points = c(
          "一张包含二分类结局和候选特征的表。",
          "也可以是表达特征与分组信息的组合。"
        ),
        studio_points = c(
          "任务类型选择 Diagnostic。",
          "可在任务备注中补充 ROC 等输出要求。",
          "把 outcome 映射到结局列。",
          "把特征映射到 covariates。"
        ),
        code_points = c(
          "优先生成 med_diagnostic_model() 脚本。",
          "可包含模型、性能输出和 ROC 相关结果。"
        ),
        caution_points = c(
          "结局标签必须足够清晰。",
          "特征数量很大时，建议先做人工筛选再生成脚本。"
        )
      )
    )
  )
}

mod_cases_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      go_studio = shiny::reactive(input$go_studio),
      go_docs = shiny::reactive(input$go_docs)
    )
  })
}
