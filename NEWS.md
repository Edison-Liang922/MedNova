# MedNova 1.0.0

## 公开发布版本

- 发布统一的 MedNova 平台入口，支持通过 `run_mednova_app()` 启动单一 Shiny 平台应用。
- 提供平台首页、平台介绍、Studio 工作台、文档中心、教程案例和 FAQ 页面。
- 提供统一分析入口：`med_psm()`、`med_logistic()`、`med_cox()`、`med_bulk_deg()`、`med_mr()`、`med_diagnostic_model()`。
- 提供任务规划与脚本生成入口：`mednova_task_assistant()`、`mednova_generate_script()`、`run_mednova_app()`。
- Studio 保持“任务配置 + 代码生成”工作流，不自动执行分析。
- 当前公开版本支持 6 类任务：propensity score matching、logistic regression、Cox regression、bulk RNA differential expression、Mendelian randomization、diagnostic modeling。
