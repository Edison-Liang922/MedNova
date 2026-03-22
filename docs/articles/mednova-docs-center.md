# 文档中心

## MedNova Docs

MedNova 的主接口、数据要求、任务输入说明和工作台用法都集中收录在这里。

## 平台简介

MedNova 是一个面向医学研究任务的智能分析平台，核心目标是把“数据对象 +
任务类型 / 任务描述 + 列映射配置”转换为：

- 结构化流程建议
- 推荐函数说明
- 可执行 R 脚本文本

平台中的 `MedNova Studio` 是交互式工作台，`MedNova Engine`
负责底层任务匹配与脚本拼装，`MedNova Docs` 负责说明与文档组织。

## 平台信息架构

MedNova 对外由四个主要部分组成：

- MedNova Studio：交互式工作台
- MedNova Engine：任务匹配、流程规划与脚本生成引擎
- MedNova Docs：文档中心
- MedNova Cases：案例与学习中心

## 建议阅读路径

如果你是第一次接触 MedNova，建议按下面顺序阅读：

1.  阅读“快速开始”，先理解输入、输出和最小接口。
2.  阅读“MedNova
    Studio：交互式工作台”，查看任务类型和当前任务输入的配置方式。
3.  阅读“能力模块总览”，确认任务是否在平台支持范围内。
4.  阅读“教程案例中心”，查看典型使用情境。
5.  如有具体问题，再查看 FAQ 和函数参考。

## 安装与启动

### 安装

当前项目适合作为本地开发版或平台原型使用。安装方式可根据你的工作流选择：

``` r

# 例如在项目根目录下
devtools::load_all()
```

### 启动 MedNova Studio

``` r

run_mednova_app()
```

[`run_mednova_app()`](https://example.com/MedNova/reference/run_mednova_app.md)
会启动平台中的交互式工作台，而不是自动执行分析。

## 文档中心内容

### 快速开始

适合第一次使用平台的用户，重点说明：

- 最小接口
- 常见输入
- 返回结构
- 列映射建议

### 统一分析入口

平台优先展示以下统一入口函数：

- [`med_psm()`](https://example.com/MedNova/reference/med_psm.md)
- [`med_logistic()`](https://example.com/MedNova/reference/med_logistic.md)
- [`med_cox()`](https://example.com/MedNova/reference/med_cox.md)
- [`med_bulk_deg()`](https://example.com/MedNova/reference/med_bulk_deg.md)
- [`med_mr()`](https://example.com/MedNova/reference/med_mr.md)
- [`med_diagnostic_model()`](https://example.com/MedNova/reference/med_diagnostic_model.md)

### Studio 工作台

适合希望通过网页方式生成脚本的用户，重点说明：

- 文件上传
- 任务类型
- 任务备注
- 当前任务输入
- 信息区解读
- 代码导出

### 数据要求

平台当前常见的输入结构包括：

- `clinical_table`
- `expression_data`
- `gwas_summary`

不同任务对输入结构的最低要求不同，例如：

- PSM 需要治疗 / 暴露列与协变量
- Logistic 需要二分类结局列与预测变量
- Cox 需要时间列、事件列与预测变量
- MR 需要 GWAS 汇总统计核心字段

### 列映射说明

平台优先使用用户在 Studio 中显式配置的列角色。

常见角色包括：

- `treatment`
- `outcome`
- `time`
- `event`
- `sample_id`
- `group`
- `covariates`

### 当前任务输入

Studio 中的输入项由任务类型决定：

- PSM：治疗 / 暴露列、协变量 / 预测变量
- Logistic：结局列、协变量 / 预测变量
- Cox：时间列、事件列、协变量 / 预测变量
- Bulk DEG：分组列、样本 ID
- MR：GWAS 关键字段
- Diagnostic：结局 / 分组列、预测变量、样本 ID

### App 使用说明

在 Studio 中，推荐的基本使用顺序是：

1.  上传数据文件
2.  选择任务类型
3.  填写任务备注与数据对象名
4.  配置当前任务输入
5.  点击“生成代码”
6.  查看输入数据信息、函数处理流程、函数输入要求与脚本

### 函数参考

适合需要查看具体函数签名、参数名和返回结构的用户。函数名、参数名和代码保持英文，以保证使用时可直接复制执行。

## 平台原则

MedNova 文档中心围绕以下原则组织内容：

- 平台只生成代码，不自动执行分析
- 平台优先展示输入要求与缺失项
- Studio 中的列映射优先级高于默认建议
- 文档中心强调真实使用场景，而不仅仅是开发说明
