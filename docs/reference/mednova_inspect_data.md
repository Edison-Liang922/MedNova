# 检查数据对象的轻量元信息

检查数据对象的轻量元信息

## Usage

``` r
mednova_inspect_data(data, data_name = NULL)
```

## Arguments

- data:

  用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。

- data_name:

  可选对象名，用于生成脚本时引用。

## Value

返回一个 list，包含对象类型、维度、列名、数值列、字符列、
启发式列角色猜测以及粗略结构判断。
