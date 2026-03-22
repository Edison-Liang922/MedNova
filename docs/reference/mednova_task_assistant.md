# 规划医学研究任务并生成 R 脚本

MedNova 不会自动执行分析。这个函数只负责检查输入对象、匹配任务域、
推荐函数，并返回可直接运行的 R 脚本文本。

## Usage

``` r
mednova_task_assistant(
  data,
  task,
  spec = list(),
  data_name = "input_data",
  output = c("list", "script"),
  save_to = NULL
)
```

## Arguments

- data:

  用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。

- task:

  自然语言任务描述。

- spec:

  可选的命名 list，用于提供任务相关提示，例如 `outcome`、
  `treatment`、`covariates`、`time`、`event`、`group`、
  `case_level`、`control_level`、`col_data_name`、 `exposure_data_name`
  或 `outcome_data_name`。

- data_name:

  脚本中使用的数据对象名。优先级依次为：显式 `data_name`， 然后是
  `spec$data_name`，最后回退到 `"input_data"`。

- output:

  输出模式。`"list"` 返回结构化结果；`"script"` 只返回脚本字符串。

- save_to:

  可选文件路径，用于保存生成的脚本。

## Value

当 `output = "list"` 时，返回包含 `data_profile`、
`matched_domain`、`selected_functions`、`workflow_steps`、 `assumptions`
与 `script` 的 list；当 `output = "script"` 时， 返回单个脚本字符串。
