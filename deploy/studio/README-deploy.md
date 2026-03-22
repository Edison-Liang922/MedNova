# MedNova Studio 部署入口

推荐优先使用仓库根目录的 `app.R` 作为 shinyapps.io 发布入口。

这个 `deploy/studio/` 目录保留为备用入口，适合本地单独测试 Studio。

## 推荐发布方式

仓库根目录已经提供：

```r
app.R
```

这个入口会在当前 bundle 根目录执行：

```r
pkgload::load_all(".")
```

然后直接启动 MedNova Studio。

推荐发布命令：

```r
rsconnect::deployApp(appDir = ".", appName = "mednova-studio")
```

## 本地测试

推荐优先直接在仓库根目录运行：

```r
install.packages("pkgload")
shiny::runApp(".")
```

如果你仍然想单独测试备用入口，也可以运行：

```r
install.packages("pkgload")
shiny::runApp("deploy/studio")
```

## shinyapps.io 发布

推荐命令是：

```r
rsconnect::deployApp(appDir = ".", appName = "mednova-studio")
```

## 发布前确认

1. 仓库根目录的 `app.R` 可以在本地正常启动。
2. 依赖包已安装：`shiny`、`bslib`、`DT`、`readxl`、`pkgload`。
3. 部署 bundle 中包含完整的 MedNova 源码目录。

## 入口逻辑

仓库根目录的 `app.R` 不依赖预先安装好的 MedNova 包，而是直接从当前 bundle 根目录加载源码。

`deploy/studio/app.R` 仍可继续使用，但更适合作为备用入口。

## 当前已知限制

1. 根目录 `app.R` 只启动 Studio 工作台，不包含平台首页、文档中心和案例页。
2. 上传 `.xlsx` 文件时需要 `readxl`。
3. 根目录部署依赖 `pkgload` 在 shinyapps.io 上可安装。
