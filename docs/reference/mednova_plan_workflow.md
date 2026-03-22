# 为支持的任务创建最小流程方案

为支持的任务创建最小流程方案

## Usage

``` r
mednova_plan_workflow(task, data_profile = NULL)
```

## Arguments

- task:

  任务描述，或者
  [`mednova_match_task()`](https://example.com/MedNova/reference/mednova_match_task.md)
  的返回结果。

- data_profile:

  可选的
  [`mednova_inspect_data()`](https://example.com/MedNova/reference/mednova_inspect_data.md)
  输出结果。

## Value

返回一个 list，包含 `workflow_steps`、`data_requirements` 与
`selected_functions`。
