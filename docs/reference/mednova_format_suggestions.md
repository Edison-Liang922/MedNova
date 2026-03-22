# 为 MedNova 任务生成最小格式建议

根据数据概况与匹配到的任务域，生成轻量级格式建议。这个辅助函数主要面向
MedNova Studio 等说明层使用场景，在不改变核心任务助手流程的前提下提供
简洁提示。

## Usage

``` r
mednova_format_suggestions(
  data_profile,
  matched_domain = NULL,
  selected_functions = NULL
)
```

## Arguments

- data_profile:

  [`mednova_inspect_data()`](https://example.com/MedNova/reference/mednova_inspect_data.md)
  返回的 list。

- matched_domain:

  MedNova 支持的任务域之一。

- selected_functions:

  可选的数据框，表示当前选中的推荐函数。

## Value

返回一个 list，包含 `structure_guess`、`suggested_column_map`、
`missing_roles` 与 `notes`。
