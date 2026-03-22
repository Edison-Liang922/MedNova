.mednova_about_card <- function(title, text) {
  shiny::div(
    class = "platform-card",
    shiny::tags$h3(title),
    shiny::tags$p(text)
  )
}

.mednova_about_feature_card <- function(title, text) {
  shiny::div(
    class = "platform-feature-card",
    shiny::tags$h3(title),
    shiny::tags$p(text)
  )
}

mod_about_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "platform-page platform-page--about",
    shiny::div(
      class = "platform-page-header",
      shiny::tags$span(class = "platform-kicker", "平台介绍"),
      shiny::tags$h2("MedNova 连接研究任务、函数入口和可执行代码"),
      shiny::tags$p(
        class = "platform-lead",
        "MedNova 面向医学研究中的常见分析任务，把数据、任务类型、当前任务输入和统一入口函数组织成清晰的工作链。"
      ),
      shiny::div(
        class = "platform-button-row",
        shiny::actionButton(ns("go_studio"), "进入 Studio", class = "btn-primary"),
        shiny::actionButton(ns("go_docs"), "查看文档中心", class = "btn-default"),
        shiny::actionButton(ns("go_cases"), "浏览教程案例", class = "btn-default")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("MedNova 是什么"),
        shiny::tags$p("这是一个面向医学研究任务的智能分析平台，不是自动分析执行器。")
      ),
      shiny::div(
        class = "platform-card-grid platform-card-grid--value",
        .mednova_about_card(
          "平台入口",
          "统一入口 app 将首页、平台介绍、Studio、文档中心、教程案例和 FAQ 放在同一处切换。"
        ),
        .mednova_about_card(
          "分析引擎",
          "平台根据任务类型、列配置和统一入口函数生成流程说明与 R 脚本。"
        ),
        .mednova_about_card(
          "工作台体验",
          "Studio 页面围绕任务配置、输入核对和代码导出展开，避免把界面做成复杂判断器。"
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台解决什么问题"),
        shiny::tags$p("很多团队并不缺统计方法，缺的是把任务、输入和函数入口整理成一致脚本的过程。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_about_feature_card(
          "从任务到流程",
          "选择任务类型后，平台直接给出对应处理步骤和主函数。"
        ),
        .mednova_about_feature_card(
          "从输入到配置",
          "界面只展示当前任务真正需要的输入项，减少无关字段干扰。"
        ),
        .mednova_about_feature_card(
          "从函数到代码",
          "脚本优先调用 `med_*` 统一入口函数，便于直接纳入真实项目。"
        )
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("支持任务"),
        shiny::tags$p("当前覆盖六类医学研究常见任务和对应的常用图表输出。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_about_feature_card("倾向评分匹配", "支持匹配、baseline table、balance table 和 love plot 输出。"),
        .mednova_about_feature_card("Logistic 回归", "支持单因素 / 多因素回归与 OR 风格结果输出。"),
        .mednova_about_feature_card("Cox 回归", "支持多因素 Cox 建模和风险比结果。"),
        .mednova_about_feature_card("差异表达分析", "支持 Bulk RNA 的分组整理、过滤、差异分析和结果查看。"),
        .mednova_about_feature_card("孟德尔随机化", "支持 exposure / outcome 准备、协调和 MR 主流程。"),
        .mednova_about_feature_card("诊断模型构建", "支持诊断建模、性能输出与 ROC 相关结果。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("平台组成"),
        shiny::tags$p("首页负责引导，介绍页负责说明，Studio 负责生成代码。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_about_card("MedNova Studio", "上传数据、选择任务类型、填写任务备注、配置当前任务输入并生成代码。"),
        .mednova_about_card("MedNova Engine", "负责 `mednova_task_assistant()`、`mednova_generate_script()` 和 `med_*` 统一入口的串联。"),
        .mednova_about_card("MedNova Docs", "说明主接口、数据要求、当前任务输入和工作台用法。"),
        .mednova_about_card("MedNova Cases", "展示 PSM、Logistic、Cox、MR 和诊断模型的典型使用场景。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("典型使用流程"),
        shiny::tags$p("常见使用路径从数据上传开始，到脚本导出结束。")
      ),
      shiny::tags$ol(
        class = "platform-step-list",
        shiny::tags$li("上传数据文件。"),
        shiny::tags$li("选择任务类型。"),
        shiny::tags$li("填写任务备注并指定数据对象名。"),
        shiny::tags$li("配置当前任务输入。"),
        shiny::tags$li("查看函数处理流程和函数输入要求。"),
        shiny::tags$li("生成并下载 R 脚本。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("数据输入与列配置"),
        shiny::tags$p("平台围绕任务需求组织输入项，不把所有角色一次性堆到页面上。")
      ),
      shiny::div(
        class = "platform-card-grid",
        .mednova_about_card("临床表任务", "PSM、Logistic、Cox 和诊断模型通常需要治疗列、结局列、时间列、事件列或预测变量。"),
        .mednova_about_card("表达数据任务", "Bulk RNA 差异表达通常需要分组列，并按需要补充样本 ID。"),
        .mednova_about_card("GWAS 汇总统计任务", "MR 场景通常需要 SNP、beta、se、pval、等位基因和样本量等关键字段。")
      ),
      shiny::div(
        class = "platform-note-card",
        shiny::tags$h3("当前任务输入"),
        shiny::tags$p("PSM 显示治疗列与协变量，Logistic 显示结局列与预测变量，Cox 显示时间列、事件列与预测变量。"),
        shiny::tags$p("Bulk、MR 和诊断模型也只展示当前任务真正需要的输入项。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("统一公开接口"),
        shiny::tags$p("平台对外优先展示 `med_*` 统一入口、任务助手和平台启动函数。")
      ),
      shiny::div(
        class = "platform-chip-row",
        shiny::tags$span(class = "chip", "med_psm"),
        shiny::tags$span(class = "chip", "med_logistic"),
        shiny::tags$span(class = "chip", "med_cox"),
        shiny::tags$span(class = "chip", "med_bulk_deg"),
        shiny::tags$span(class = "chip", "med_mr"),
        shiny::tags$span(class = "chip", "med_diagnostic_model"),
        shiny::tags$span(class = "chip", "mednova_task_assistant"),
        shiny::tags$span(class = "chip", "mednova_generate_script"),
        shiny::tags$span(class = "chip", "run_mednova_app")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("为什么只生成代码"),
        shiny::tags$p("研究设计、变量含义、缺失处理和统计解释仍然需要人工确认。")
      ),
      shiny::div(
        class = "platform-note-card",
        shiny::tags$p("MedNova 负责把任务配置、函数入口和脚本骨架组织清楚。"),
        shiny::tags$p("是否执行分析、参数如何调整、结果如何解释，仍然由研究者或统计人员决定。")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("适用场景"),
        shiny::tags$p("适合项目启动、脚本骨架生成、教学示例和团队协作中的输入规范整理。")
      ),
      shiny::div(
        class = "platform-chip-row",
        shiny::tags$span(class = "chip", "临床研究启动"),
        shiny::tags$span(class = "chip", "统计脚本草拟"),
        shiny::tags$span(class = "chip", "团队输入对齐"),
        shiny::tags$span(class = "chip", "研究教学示例"),
        shiny::tags$span(class = "chip", "方法流程标准化")
      )
    ),
    shiny::div(
      class = "platform-section",
      shiny::div(
        class = "platform-section__header",
        shiny::tags$h2("工作台预览"),
        shiny::tags$p("Studio 页面承担任务配置、输入核对和代码导出，不承担首页门面职责。")
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
          shiny::tags$h3("继续进入平台核心内容"),
          shiny::tags$p("可以直接进入工作台，也可以继续查看文档中心和案例页。")
        ),
        shiny::div(
          class = "platform-button-row",
          shiny::actionButton(ns("go_studio_bottom"), "进入 Studio", class = "btn-primary"),
          shiny::actionButton(ns("go_docs_bottom"), "查看文档中心", class = "btn-default"),
          shiny::actionButton(ns("go_cases_bottom"), "浏览教程案例", class = "btn-default")
        )
      )
    )
  )
}

mod_about_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    list(
      go_studio = shiny::reactive(input$go_studio + input$go_studio_bottom),
      go_docs = shiny::reactive(input$go_docs + input$go_docs_bottom),
      go_cases = shiny::reactive(input$go_cases + input$go_cases_bottom)
    )
  })
}
