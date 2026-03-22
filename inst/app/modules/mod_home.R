.mednova_home_card <- function(title, text) {
  shiny::div(
    class = "platform-card",
    shiny::tags$h3(title),
    shiny::tags$p(text)
  )
}

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "platform-page platform-page--home",
    shiny::div(
      class = "platform-hero",
      shiny::div(
        class = "platform-hero__content",
        shiny::tags$span(class = "platform-hero__eyebrow", "MedNova Platform"),
        shiny::tags$h1("MedNova"),
        shiny::tags$p(
          class = "platform-hero__subtitle",
          "面向医学研究任务的智能分析平台"
        ),
        shiny::tags$p(
          class = "platform-hero__lead",
          "将用户数据、任务目标与列配置转换为流程说明、函数说明和可执行 R 脚本。"
        ),
        shiny::div(
          class = "platform-button-row",
          shiny::actionButton(ns("go_about"), "了解 MedNova", class = "btn-primary"),
          shiny::actionButton(ns("go_studio"), "进入 Studio", class = "btn-default"),
          shiny::actionButton(ns("go_docs"), "查看文档中心", class = "btn-default"),
          shiny::actionButton(ns("go_cases"), "浏览教程案例", class = "btn-default")
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台价值"),
        shiny::tags$p("把医学研究中的任务配置、函数要求和代码输出放到同一条工作链上。")
      ),
      shiny::div(
        class = "platform-card-grid platform-card-grid--value",
        .mednova_home_card(
          "从任务到流程",
          "按任务类型整理处理步骤，直接给出可落地的分析骨架。"
        ),
        .mednova_home_card(
          "从输入到配置",
          "围绕当前任务显示必需输入项，方便逐项核对。"
        ),
        .mednova_home_card(
          "从函数到代码",
          "优先调用 MedNova 统一入口函数，生成可直接运行的 R 脚本。"
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台能力"),
        shiny::tags$p("当前支持医学研究中常见的六类任务和常用可视化输出。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_home_card("倾向评分匹配", "支持匹配、平衡核对、baseline table 和 love plot 输出。"),
        .mednova_home_card("Logistic 回归", "支持二分类结局建模、单因素 / 多因素结果和 OR 风格输出。"),
        .mednova_home_card("Cox 回归", "支持时间到事件建模与风险比结果输出。"),
        .mednova_home_card("差异表达分析", "支持 Bulk RNA 分组整理、差异分析和结果查看。"),
        .mednova_home_card("孟德尔随机化", "支持 exposure / outcome 数据准备、协调和 MR 主流程。"),
        .mednova_home_card("诊断模型构建", "支持模型、性能输出与 ROC 相关结果。"),
        .mednova_home_card("可视化输出", "支持 love plot、forest plot、ROC 和火山图等常见图形。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台结构"),
        shiny::tags$p("平台入口、分析引擎、文档和案例都在同一个应用中切换。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_home_card("MedNova Studio", "交互式工作台，用于上传数据、选择任务、配置当前任务输入并生成代码。"),
        .mednova_home_card("MedNova Engine", "负责任务匹配、流程组织、统一入口调用和脚本拼装。"),
        .mednova_home_card("MedNova Docs", "集中说明主接口、数据要求、任务输入和工作台用法。"),
        .mednova_home_card("MedNova Cases", "提供 PSM、Logistic、Cox、MR 和诊断模型的场景示例。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("使用流程"),
        shiny::tags$p("从上传数据到下载脚本，流程清晰，适合快速进入分析准备阶段。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_home_card("1. 上传数据", "导入 `.csv`、`.tsv`、`.xlsx` 或 `.rds` 文件。"),
        .mednova_home_card("2. 选择任务", "以任务类型作为主入口，决定当前工作流和输入项。"),
        .mednova_home_card("3. 配置当前任务输入", "只填写当前任务真正需要的列和参数。"),
        .mednova_home_card("4. 核对函数输入", "查看主函数、当前输入和缺失项。"),
        .mednova_home_card("5. 生成并下载 R 脚本", "导出完整脚本，进入项目环境继续人工复核与执行。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("工作台预览"),
        shiny::tags$p("Studio 页面专注任务配置和代码生成，不承担首页职责。")
      ),
      shiny::div(
        class = "platform-preview-panel",
        shiny::div(
          class = "platform-mockup",
          shiny::div(
            class = "platform-mockup__bar",
            shiny::tags$span(),
            shiny::tags$span(),
            shiny::tags$span()
          ),
          shiny::div(
            class = "platform-mockup__body",
            shiny::div(
              class = "platform-mockup__sidebar",
              shiny::div(class = "platform-mockup__line platform-mockup__line--lg"),
              shiny::div(class = "platform-mockup__line"),
              shiny::div(class = "platform-mockup__line"),
              shiny::div(class = "platform-mockup__line")
            ),
            shiny::div(
              class = "platform-mockup__main",
              shiny::div(
                class = "platform-mockup__panel platform-mockup__panel--light",
                shiny::div(class = "platform-mockup__line platform-mockup__line--soft"),
                shiny::div(class = "platform-mockup__line platform-mockup__line--soft"),
                shiny::div(class = "platform-mockup__line platform-mockup__line--soft")
              ),
              shiny::div(
                class = "platform-mockup__panel platform-mockup__panel--dark",
                shiny::div(class = "platform-mockup__code"),
                shiny::div(class = "platform-mockup__code"),
                shiny::div(class = "platform-mockup__code"),
                shiny::div(class = "platform-mockup__code")
              )
            )
          )
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-cta-banner",
        shiny::div(
          class = "platform-cta-banner__text",
          shiny::tags$h3("继续进入平台核心页面"),
          shiny::tags$p("Studio、文档中心和教程案例都在同一个平台中切换。")
        ),
        shiny::div(
          class = "platform-button-row",
          shiny::actionButton(ns("go_about_bottom"), "了解 MedNova", class = "btn-default"),
          shiny::actionButton(ns("go_studio_bottom"), "进入 Studio", class = "btn-primary"),
          shiny::actionButton(ns("go_docs_bottom"), "查看文档中心", class = "btn-default"),
          shiny::actionButton(ns("go_cases_bottom"), "浏览教程案例", class = "btn-default")
        )
      )
    )
  )
}

mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      go_about = shiny::reactive(input$go_about + input$go_about_bottom),
      go_studio = shiny::reactive(input$go_studio + input$go_studio_bottom),
      go_docs = shiny::reactive(input$go_docs + input$go_docs_bottom),
      go_cases = shiny::reactive(input$go_cases + input$go_cases_bottom)
    )
  })
}
