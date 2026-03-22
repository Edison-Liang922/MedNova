# 为支持任务生成可运行的 R 脚本

为支持任务生成可运行的 R 脚本

## Usage

``` r
mednova_generate_script(
  task,
  data,
  spec = list(),
  data_profile = NULL,
  assumptions = NULL
)
```

## Arguments

- task:

  任务描述，或者
  [`mednova_match_task()`](https://example.com/MedNova/reference/mednova_match_task.md)
  的返回结果。

- data:

  用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。

- spec:

  可选的命名 list，用于提供任务相关提示。

- data_profile:

  可选的
  [`mednova_inspect_data()`](https://example.com/MedNova/reference/mednova_inspect_data.md)
  输出结果。省略时会自动生成。

- assumptions:

  可选字符向量，用于在脚本顶部写入假设说明。

## Value

返回单个字符字符串，即生成好的 R 脚本文本。
