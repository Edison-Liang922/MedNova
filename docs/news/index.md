# Changelog

## MedNova 1.0.0

### 公开发布版本

- 发布统一的 MedNova 平台入口，支持通过
  [`run_mednova_app()`](https://example.com/MedNova/reference/run_mednova_app.md)
  启动单一 Shiny 平台应用。
- 提供平台首页、平台介绍、Studio 工作台、文档中心、教程案例和 FAQ 页面。
- 提供统一分析入口：[`med_psm()`](https://example.com/MedNova/reference/med_psm.md)、[`med_logistic()`](https://example.com/MedNova/reference/med_logistic.md)、[`med_cox()`](https://example.com/MedNova/reference/med_cox.md)、[`med_bulk_deg()`](https://example.com/MedNova/reference/med_bulk_deg.md)、[`med_mr()`](https://example.com/MedNova/reference/med_mr.md)、[`med_diagnostic_model()`](https://example.com/MedNova/reference/med_diagnostic_model.md)。
- 提供任务规划与脚本生成入口：[`mednova_task_assistant()`](https://example.com/MedNova/reference/mednova_task_assistant.md)、[`mednova_generate_script()`](https://example.com/MedNova/reference/mednova_generate_script.md)、[`run_mednova_app()`](https://example.com/MedNova/reference/run_mednova_app.md)。
- Studio 保持“任务配置 + 代码生成”工作流，不自动执行分析。
- 当前公开版本支持 6 类任务：propensity score matching、logistic
  regression、Cox regression、bulk RNA differential
  expression、Mendelian randomization、diagnostic modeling。
