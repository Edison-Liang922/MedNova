# MedNova

MedNova 是一个面向医学研究任务的智能分析平台。

## Online access / 在线入口

- Platform: <https://edison-liang922.github.io/MedNova/>
- Studio: <https://edison-liang.shinyapps.io/mednova-studio/>

平台接收：

- 用户提供的数据对象
- 用户选择的任务类型或提供的任务描述
- 用户明确配置的列映射

平台输出：

- 推荐分析流程
- 推荐调用函数
- 函数输入要求与缺失项提示
- 可直接运行的 R 脚本

MedNova 只生成代码与说明，不会自动执行分析。

## v1.0 定位

MedNova v1.0 是首个公开可用版本。

这个版本提供统一的平台入口、中文化的 Studio 工作台、文档中心与案例页面，适合用于医学研究场景下的任务配置、流程梳理和 R 脚本生成。

## 平台定位

MedNova 对外由四个主要部分组成：

- `MedNova Studio`：平台中的交互式工作台
- `MedNova Engine`：任务匹配、流程规划与脚本生成引擎
- `MedNova Docs`：平台文档中心
- `MedNova Cases`：平台教程与案例中心

## 平台当前支持任务

当前支持以下 6 类任务：

1. propensity score matching
2. logistic regression
3. Cox regression
4. bulk RNA differential expression
5. Mendelian randomization
6. diagnostic modeling

## 快速开始

```r
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

## 启动平台

安装 MedNova 后，可以直接启动平台应用：

```r
library(MedNova)
run_mednova_app()
```

平台采用统一入口 app，包含平台首页、平台介绍、Studio 工作台、文档中心、教程案例和 FAQ。

## 对外主接口

统一分析入口：

- `med_psm()`
- `med_logistic()`
- `med_cox()`
- `med_bulk_deg()`
- `med_mr()`
- `med_diagnostic_model()`

任务与脚本入口：

- `mednova_task_assistant()`
- `mednova_generate_script()`
- `run_mednova_app()`

## MedNova Studio

`run_mednova_app()` 会启动统一平台 app，其中 `MedNova Studio` 是平台中的交互式工作台。

在 Studio 中，你可以：

- 上传 `.csv`、`.tsv`、`.xlsx` 或 `.rds` 数据
- 选择任务类型
- 填写任务备注（可选）
- 配置当前任务输入
- 查看输入数据信息、函数处理流程与函数输入要求
- 复制或下载生成的 `.R` 脚本

启动方式：

```r
run_mednova_app()
```

## 平台工作流

平台建议的使用路径如下：

1. 上传数据
2. 选择任务
3. 配置当前任务输入
4. 核对函数输入
5. 生成代码

## 数据要求

不同任务对输入结构的最低要求不同：

- `propensity score matching`：临床表，至少明确治疗 / 暴露列与协变量
- `logistic regression`：临床表，至少明确二分类结局列与预测变量
- `Cox regression`：临床表，至少明确时间列、事件列与预测变量
- `bulk RNA differential expression`：表达矩阵与样本元数据，至少明确分组列，最好有样本 ID
- `Mendelian randomization`：暴露与结局的 GWAS 汇总统计
- `diagnostic modeling`：二分类结局与候选特征

## 列映射说明

在平台中，列角色映射优先由用户显式指定。当前常用角色包括：

- 治疗 / 暴露列
- 结局列
- 时间列
- 事件列
- 样本 ID 列
- 分组列
- 协变量 / 预测变量

在 Studio 中，当前任务输入由任务类型决定，最终脚本优先使用用户实际选择的映射结果。

## 文档中心与案例中心

平台同时提供两类内容入口：

- 文档中心：查看快速开始、Studio 工作台说明、函数参考与规则说明
- 教程案例中心：查看典型使用场景、学习路径与案例页面

## 常见问题

### MedNova 会自动运行分析吗？

不会。平台只做任务规划、函数推荐与脚本生成。

### 平台生成的脚本还需要人工检查吗？

需要。当前实现以“最小可运行”为目标，但仍建议根据真实研究设计做人工复核。

### 如果我的任务超出当前平台范围怎么办？

建议先收敛到当前支持的任务域，或者把平台当作脚本骨架生成器，再人工补充更复杂的下游部分。
