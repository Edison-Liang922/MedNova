# 启动 MedNova 平台

启动统一的 MedNova Shiny 平台入口，在同一个应用中访问平台首页、
平台介绍、Studio 工作台、函数与文档、教程案例和 FAQ。

## Usage

``` r
run_mednova_app(
  launch.browser = interactive(),
  host = "127.0.0.1",
  port = NULL,
  display.mode = c("auto", "normal", "showcase"),
  return_app = FALSE
)
```

## Arguments

- launch.browser:

  是否在浏览器中打开应用。

- host:

  传递给 [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  的 host。

- port:

  传递给 [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  的 port。为 `NULL` 时由 Shiny 自行处理。

- display.mode:

  传递给 [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)
  的显示模式。

- return_app:

  逻辑值；当 `TRUE` 时返回 Shiny app 对象而不直接启动。 这个参数主要用于
  smoke test。

## Value

不显式返回时会启动应用；当 `return_app = TRUE` 时返回 一个
`shiny.appobj`。
