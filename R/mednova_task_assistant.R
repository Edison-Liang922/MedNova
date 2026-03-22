#' 规划医学研究任务并生成 R 脚本
#'
#' MedNova 不会自动执行分析。这个函数只负责检查输入对象、匹配任务域、
#' 推荐函数，并返回可直接运行的 R 脚本文本。
#'
#' @param data 用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。
#' @param task 自然语言任务描述。
#' @param spec 可选的命名 list，用于提供任务相关提示，例如 `outcome`、
#'   `treatment`、`covariates`、`time`、`event`、`group`、
#'   `case_level`、`control_level`、`col_data_name`、
#'   `exposure_data_name` 或 `outcome_data_name`。
#' @param data_name 脚本中使用的数据对象名。优先级依次为：显式 `data_name`，
#'   然后是 `spec$data_name`，最后回退到 `"input_data"`。
#' @param output 输出模式。`"list"` 返回结构化结果；`"script"` 只返回脚本字符串。
#' @param save_to 可选文件路径，用于保存生成的脚本。
#'
#' @return 当 `output = "list"` 时，返回包含 `data_profile`、
#'   `matched_domain`、`selected_functions`、`workflow_steps`、
#'   `assumptions` 与 `script` 的 list；当 `output = "script"` 时，
#'   返回单个脚本字符串。
#' @export
mednova_task_assistant <- function(data,
                                   task,
                                   spec = list(),
                                   data_name = "input_data",
                                   output = c("list", "script"),
                                   save_to = NULL) {
  output <- match.arg(output)

  if (missing(data)) {
    stop("`data` must be provided.", call. = FALSE)
  }

  if (!is.character(task) || length(task) != 1L || !nzchar(trimws(task))) {
    stop("`task` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.list(spec)) {
    stop("`spec` must be a named list.", call. = FALSE)
  }

  resolved_data_name <- .mednova_choose_data_name(
    data_name = data_name,
    spec_data_name = spec$data_name,
    data_name_missing = missing(data_name)
  )

  if (!is.null(save_to) &&
      (!is.character(save_to) || length(save_to) != 1L || !nzchar(trimws(save_to)))) {
    stop("`save_to` must be a non-empty file path.", call. = FALSE)
  }

  data_profile <- mednova_inspect_data(data, data_name = resolved_data_name)
  task_match <- mednova_match_task(task)
  workflow <- mednova_plan_workflow(task_match, data_profile = data_profile)
  assumptions <- .mednova_build_assumptions(
    domain = task_match$domain,
    data = data,
    spec = spec,
    data_profile = data_profile
  )
  script <- mednova_generate_script(
    task = task_match,
    data = data,
    spec = spec,
    data_profile = data_profile,
    assumptions = assumptions
  )

  if (!is.null(save_to)) {
    dir.create(dirname(save_to), recursive = TRUE, showWarnings = FALSE)
    writeLines(script, con = save_to, useBytes = TRUE)
  }

  if (identical(output, "script")) {
    return(script)
  }

  structure(
    list(
      data_profile = data_profile,
      matched_domain = task_match$domain,
      selected_functions = task_match$selected_functions,
      workflow_steps = workflow$workflow_steps,
      assumptions = assumptions,
      script = script
    ),
    class = "mednova_result"
  )
}

.mednova_choose_data_name <- function(data_name,
                                      spec_data_name = NULL,
                                      data_name_missing = TRUE) {
  if (!isTRUE(data_name_missing)) {
    .mednova_validate_data_name(data_name)
    return(data_name)
  }

  if (is.character(spec_data_name) &&
      length(spec_data_name) == 1L &&
      nzchar(trimws(spec_data_name))) {
    return(spec_data_name)
  }

  "input_data"
}

.mednova_validate_data_name <- function(data_name) {
  if (!is.character(data_name) || length(data_name) != 1L || !nzchar(trimws(data_name))) {
    stop("`data_name` must be a non-empty character string.", call. = FALSE)
  }
}
