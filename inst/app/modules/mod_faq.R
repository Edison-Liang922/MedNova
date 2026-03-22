.mednova_faq_item <- function(question, answer) {
  shiny::tags$details(
    class = "platform-faq-item",
    shiny::tags$summary(question),
    shiny::tags$div(
      class = "platform-faq-item__body",
      answer
    )
  )
}

mod_faq_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "platform-page platform-page--faq",
    shiny::div(
      class = "platform-page-header",
      shiny::tags$span(class = "platform-kicker", "FAQ"),
      shiny::tags$h2("常见问题"),
      shiny::tags$p(
        class = "platform-lead",
        "平台使用中最常见的问题集中收录在这一处。"
      ),
      shiny::div(
        class = "platform-button-row",
        shiny::actionButton(ns("go_docs"), "查看文档中心", class = "btn-default"),
        shiny::actionButton(ns("go_studio"), "进入 Studio 工作台", class = "btn-primary")
      )
    ),
    shiny::div(
      class = "platform-faq-list",
      .mednova_faq_item(
        "beta / se / pval / n 是什么？",
        shiny::tagList(
          shiny::tags$p("这些字段常见于 GWAS summary 或 MR 数据："),
          shiny::tags$ul(
            class = "platform-mini-list",
            shiny::tags$li("beta：效应值。"),
            shiny::tags$li("se：标准误。"),
            shiny::tags$li("pval：P 值。"),
            shiny::tags$li("n：样本量。")
          ),
          shiny::tags$p("如果你的数据是普通临床表，通常不需要同时具备这些字段。")
        )
      ),
      .mednova_faq_item(
        "为什么提示缺少某些列？",
        shiny::tagList(
          shiny::tags$p("平台会根据任务类型和当前主函数列出必需输入。"),
          shiny::tags$p("如果你还没有完成列选择，或者数据里本来就没有对应字段，就会看到缺失提示。")
        )
      ),
      .mednova_faq_item(
        "结局列和事件列有什么区别？",
        shiny::tagList(
          shiny::tags$p("结局列通常用于 Logistic 回归或诊断模型，表示样本最终属于哪一类。"),
          shiny::tags$p("事件列通常用于 Cox 回归，表示随访期间是否发生事件，一般要与时间列一起使用。")
        )
      ),
      .mednova_faq_item(
        "为什么 MedNova 只生成代码，不自动执行分析？",
        shiny::tagList(
          shiny::tags$p("MedNova 只生成代码，分析执行仍由研究者掌控。"),
          shiny::tags$p("这样可以保留研究人员对变量含义、数据质量、统计策略和结果解释的最终控制权。")
        )
      ),
      .mednova_faq_item(
        "生成的代码还需要手动修改吗？",
        shiny::tagList(
          shiny::tags$p("通常需要。建议至少人工检查以下内容："),
          shiny::tags$ul(
            class = "platform-mini-list",
            shiny::tags$li("变量是否真的符合研究设计。"),
            shiny::tags$li("分组、事件或时间定义是否正确。"),
            shiny::tags$li("协变量是否需要调整、筛选或补充。"),
            shiny::tags$li("输出路径、对象命名和绘图参数是否符合你的项目规范。")
          )
        )
      ),
      .mednova_faq_item(
        "clinical table / expression data / gwas summary 的区别是什么？",
        shiny::tagList(
          shiny::tags$ul(
            class = "platform-mini-list",
            shiny::tags$li("clinical table：患者级或样本级表格，常见于 PSM、Logistic、Cox 与诊断模型。"),
            shiny::tags$li("expression data：表达矩阵或 counts + 元数据结构，常见于 bulk RNA 差异表达。"),
            shiny::tags$li("gwas summary：SNP 级汇总统计，常见于 MR。")
          ),
          shiny::tags$p("这些输入结构主要用于理解不同任务对数据的要求。")
        )
      )
    )
  )
}

mod_faq_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      go_docs = shiny::reactive(input$go_docs),
      go_studio = shiny::reactive(input$go_studio)
    )
  })
}
