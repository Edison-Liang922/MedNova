# 将任务描述匹配到 MedNova 支持的任务域

将任务描述匹配到 MedNova 支持的任务域

## Usage

``` r
mednova_match_task(task)
```

## Arguments

- task:

  自然语言任务描述。

## Value

返回一个 list，包含 `domain`、`template_id`、
`selected_functions`、`task_label`、`matched_terms` 与 `confidence`。
