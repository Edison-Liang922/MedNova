# 文档中心：快速开始

## 简介

MedNova 的最基础上手路径在这里。

MedNova 是一个面向医学研究任务的智能分析平台，用于把“数据对象 +
任务描述”转换为流程建议、函数说明与可执行 R
脚本。平台不会自动执行分析，而是把任务规划与代码生成整理得更直接。

## 支持任务

当前支持以下 6 类任务：

1.  propensity score matching
2.  logistic regression
3.  Cox regression
4.  bulk RNA differential expression
5.  Mendelian randomization
6.  diagnostic modeling

## 最小使用方式

``` r

library(MedNova)

result <- mednova_task_assistant(
  data = analysis_data,
  task = "logistic regression for 30-day mortality",
  spec = list(
    outcome = "mortality_30d",
    covariates = c("age", "sex", "bmi")
  ),
  data_name = "analysis_data",
  output = "list"
)

cat(result$script)
```

## 平台入口

MedNova 当前包含几个面向用户的主要入口：

- [`med_psm()`](https://example.com/MedNova/reference/med_psm.md)、[`med_logistic()`](https://example.com/MedNova/reference/med_logistic.md)、[`med_cox()`](https://example.com/MedNova/reference/med_cox.md)、[`med_bulk_deg()`](https://example.com/MedNova/reference/med_bulk_deg.md)、[`med_mr()`](https://example.com/MedNova/reference/med_mr.md)、[`med_diagnostic_model()`](https://example.com/MedNova/reference/med_diagnostic_model.md)：统一分析入口
- [`mednova_task_assistant()`](https://example.com/MedNova/reference/mednova_task_assistant.md)：任务与脚本入口
- [`mednova_generate_script()`](https://example.com/MedNova/reference/mednova_generate_script.md)：脚本生成入口
- [`run_mednova_app()`](https://example.com/MedNova/reference/run_mednova_app.md)：平台中的交互式工作台入口
- MedNova Docs：平台文档中心
- MedNova Cases：平台案例与学习中心

## 输入与输出

[`mednova_task_assistant()`](https://example.com/MedNova/reference/mednova_task_assistant.md)
的最小输入包括：

- `data`：数据对象
- `task`：任务描述

可选输入包括：

- `spec`：显式列角色与任务提示
- `data_name`：在脚本中使用的数据对象名
- `output`：返回结构化结果或纯脚本字符串

默认输出包含：

- `data_profile`
- `matched_domain`
- `selected_functions`
- `workflow_steps`
- `assumptions`
- `script`

## 数据要求

不同任务的数据最低要求不同：

- 倾向评分匹配：治疗 / 暴露列 + 协变量
- Logistic 回归：二分类结局列 + 协变量
- Cox 回归：时间列 + 事件列 + 协变量
- Bulk RNA 差异表达：表达矩阵 + 样本元数据 + 分组信息
- 孟德尔随机化：暴露与结局 GWAS 汇总统计
- 诊断建模：结局列 + 特征列

## 列映射建议

如果列名不标准，建议通过 `spec` 或 MedNova Studio
显式指定列角色。平台优先把“用户明确指定”视为最终结论。

常见角色包括：

- `treatment`
- `outcome`
- `time`
- `event`
- `sample_id`
- `group`
- `covariates`

## 函数需求说明

MedNova
会根据任务匹配结果，从内置函数目录中筛出当前任务优先推荐的函数，并给出：

- 当前函数需要的输入
- 当前脚本更适合调用哪些函数
- 某些函数是否依赖中间对象或结构化输入

这一层不会执行分析，主要用于在脚本生成前核对当前数据与函数输入要求。

## 示例

### Logistic 回归

``` r

mednova_task_assistant(
  data = analysis_data,
  task = "logistic regression with odds ratio output",
  spec = list(
    outcome = "status",
    covariates = c("age", "sex", "bmi", "smoking")
  )
)
```

### Cox 回归

``` r

mednova_task_assistant(
  data = survival_data,
  task = "cox regression for overall survival",
  spec = list(
    time = "os_time",
    event = "os_event",
    covariates = c("stage", "age", "score")
  )
)
```

## 下一步阅读

如果你已经完成快速开始，建议继续阅读：

- `MedNova Studio：交互式工作台`
- `能力模块总览`
- `教程案例中心`
- `FAQ`

## 常见问题

### MedNova 会自动运行分析吗？

不会。MedNova 只返回文本形式的脚本与说明。

### 如果我不提供 `spec` 可以吗？

可以。MedNova
会使用最小启发式规则推断部分字段，但更推荐显式指定关键列角色。

### 生成脚本后还需要人工检查吗？

需要。当前实现强调“最小可运行”，并不替代研究设计审查与统计判断。
