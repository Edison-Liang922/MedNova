app_dir <- getwd()

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) {
    return(y)
  }

  x
}

req <- shiny::req

source(file.path(app_dir, "modules", "mod_upload.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_home.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_about.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_docs.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_cases.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_faq.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_profile.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_recommend.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_requirements.R"), local = TRUE)
source(file.path(app_dir, "modules", "mod_code.R"), local = TRUE)

.mednova_app_exported <- function(name) {
  if ("MedNova" %in% loadedNamespaces()) {
    return(getExportedValue("MedNova", name))
  }

  if (exists(name, envir = .GlobalEnv, inherits = TRUE)) {
    return(get(name, envir = .GlobalEnv, inherits = TRUE))
  }

  stop("当前 R 会话中无法找到 MedNova 函数 `", name, "`。", call. = FALSE)
}

.mednova_app_read_data <- function(file_info) {
  if (is.null(file_info) || !nzchar(file_info$datapath)) {
    stop("请先上传数据文件。", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(file_info$name))

  switch(
    ext,
    csv = utils::read.csv(
      file_info$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    tsv = utils::read.delim(
      file_info$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    xlsx = {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        stop("读取 .xlsx 文件需要安装 `readxl` 包。", call. = FALSE)
      }
      as.data.frame(
        readxl::read_excel(file_info$datapath),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    rds = readRDS(file_info$datapath),
    stop("暂不支持该文件类型：.", ext, call. = FALSE)
  )
}

.mednova_app_has_value <- function(x) {
  if (is.null(x) || !length(x)) {
    return(FALSE)
  }

  any(nzchar(trimws(as.character(x))))
}

.mednova_app_script_note_lines <- function(task_note) {
  if (!is.character(task_note) || length(task_note) != 1L) {
    return(character())
  }

  note <- trimws(task_note)
  if (!nzchar(note)) {
    return(character())
  }

  note_parts <- trimws(unlist(strsplit(note, "\n", fixed = TRUE), use.names = FALSE))
  note_parts <- note_parts[nzchar(note_parts)]

  if (!length(note_parts)) {
    return(character())
  }

  c("# Task note:", paste0("# - ", note_parts))
}

.mednova_app_attach_task_note <- function(script, task_note) {
  note_lines <- .mednova_app_script_note_lines(task_note)

  if (!length(note_lines) || !is.character(script) || length(script) != 1L || !nzchar(script)) {
    return(script)
  }

  lines <- strsplit(script, "\n", fixed = TRUE)[[1L]]
  insert_at <- match("", lines)

  if (is.na(insert_at)) {
    return(paste(c(lines, "", note_lines), collapse = "\n"))
  }

  updated_lines <- append(lines, values = c(note_lines, ""), after = insert_at - 1L)
  paste(updated_lines, collapse = "\n")
}

.mednova_app_empty_mapping <- function() {
  list(
    treatment = NULL,
    outcome = NULL,
    time = NULL,
    event = NULL,
    sample_id = NULL,
    group = NULL,
    covariates = character()
  )
}

.mednova_app_first_nonempty <- function(...) {
  values <- list(...)

  for (value in values) {
    if (is.character(value) && length(value) == 1L && nzchar(trimws(value))) {
      return(value)
    }
  }

  NULL
}

.mednova_app_compact_mapping <- function(column_mapping) {
  mapping <- .mednova_app_empty_mapping()

  if (is.list(column_mapping) && length(column_mapping)) {
    mapping[names(column_mapping)] <- column_mapping
  }

  compact <- list()

  for (name in names(mapping)) {
    value <- mapping[[name]]

    if (!.mednova_app_has_value(value)) {
      next
    }

    compact[[name]] <- unique(as.character(value[nzchar(trimws(as.character(value)))]))
  }

  compact
}

.mednova_app_guess_mapping <- function(data_profile) {
  mapping <- .mednova_app_empty_mapping()

  if (is.null(data_profile) || !is.list(data_profile)) {
    return(mapping)
  }

  guessed <- data_profile$guessed_columns %||% list()
  mapping$treatment <- .mednova_app_first_nonempty(
    guessed$treatment,
    guessed$exposure,
    guessed$group
  )
  mapping$outcome <- .mednova_app_first_nonempty(
    guessed$outcome,
    guessed$case_control,
    guessed$event
  )
  mapping$time <- .mednova_app_first_nonempty(guessed$time)
  mapping$event <- .mednova_app_first_nonempty(guessed$event, guessed$outcome)
  mapping$sample_id <- .mednova_app_first_nonempty(guessed$sample_id, guessed$id)
  mapping$group <- .mednova_app_first_nonempty(
    guessed$group,
    guessed$treatment,
    guessed$exposure,
    guessed$case_control
  )

  reserved <- unique(stats::na.omit(unlist(mapping[setdiff(names(mapping), "covariates")], use.names = FALSE)))
  numeric_columns <- data_profile$numeric_columns %||% character()
  column_names <- data_profile$column_names %||% character()
  suggested_covariates <- setdiff(numeric_columns, reserved)

  if (!length(suggested_covariates)) {
    suggested_covariates <- setdiff(column_names, reserved)
  }

  mapping$covariates <- utils::head(unique(suggested_covariates), 6L)
  mapping
}

.mednova_app_build_spec <- function(column_mapping) {
  mapping <- .mednova_app_compact_mapping(column_mapping)
  single_roles <- c("treatment", "outcome", "time", "event", "sample_id", "group")
  excluded <- unique(unlist(mapping[intersect(single_roles, names(mapping))], use.names = FALSE))
  covariates <- setdiff(mapping$covariates %||% character(), excluded)

  spec <- list(
    treatment = mapping$treatment,
    exposure = mapping$treatment,
    outcome = mapping$outcome,
    time = mapping$time,
    event = mapping$event,
    sample_id = mapping$sample_id,
    group = mapping$group,
    covariates = covariates,
    predictors = covariates
  )

  spec[vapply(spec, .mednova_app_has_value, logical(1L))]
}

.mednova_app_role_labels <- function() {
  c(
    treatment = "治疗 / 暴露列",
    outcome = "结局列",
    time = "时间列",
    event = "事件列",
    sample_id = "样本 ID 列",
    group = "分组列",
    covariates = "协变量 / 预测变量"
  )
}

.mednova_app_structure_label_cn <- function(value) {
  switch(
    value,
    clinical_table = "临床表（clinical_table）",
    expression_data = "表达数据（expression_data）",
    gwas_summary = "GWAS 汇总统计（gwas_summary）",
    unknown = "未知",
    value
  )
}

.mednova_app_role_label <- function(role) {
  labels <- .mednova_app_role_labels()

  if (is.character(role) && length(role) == 1L && role %in% names(labels)) {
    return(unname(labels[[role]]))
  }

  gsub("_", " ", role, fixed = TRUE)
}

.mednova_app_expected_roles_for_domain <- function(matched_domain) {
  if (is.null(matched_domain) || !nzchar(matched_domain)) {
    return(character())
  }

  switch(
    matched_domain,
    propensity_score_matching = c("treatment", "covariates"),
    logistic_regression = c("outcome", "covariates"),
    cox_regression = c("time", "event", "covariates"),
    bulk_rna_differential_expression = c("sample_id", "group"),
    mendelian_randomization = character(),
    diagnostic_modeling = c("outcome", "covariates"),
    character()
  )
}

.mednova_app_structure_note <- function(structure_guess, matched_domain) {
  if (is.null(matched_domain) || !nzchar(matched_domain)) {
    return(NULL)
  }

  if (matched_domain %in% c(
    "propensity_score_matching",
    "logistic_regression",
    "cox_regression",
    "diagnostic_modeling"
  ) && identical(structure_guess, "expression_data")) {
    return("该任务通常需要临床表格式数据，但当前上传对象更像表达数据。")
  }

  if (identical(matched_domain, "bulk_rna_differential_expression") &&
      !identical(structure_guess, "expression_data")) {
    return("Bulk RNA 差异表达通常需要表达矩阵以及配套的样本元数据。")
  }

  if (identical(matched_domain, "mendelian_randomization")) {
    return("孟德尔随机化通常需要暴露与结局的 GWAS 汇总统计，而不是单个临床表。")
  }

  NULL
}

.mednova_app_translate_structure_reason <- function(reason) {
  if (!is.character(reason) || length(reason) != 1L || !nzchar(reason)) {
    return(reason)
  }

  switch(
    reason,
    "Only an object name was supplied, so the structure could not be inspected." =
      "当前仅提供了对象名称，因此无法直接检查数据结构。",
    "Detected a list with `counts` and `col_data` components." =
      "检测到对象是一个包含 `counts` 与 `col_data` 组件的 list。",
    "Detected a list with `exposure_dat` and `outcome_dat` components." =
      "检测到对象是一个包含 `exposure_dat` 与 `outcome_dat` 组件的 list。",
    "Detected several GWAS summary statistic columns such as SNP, beta, SE, or p-value." =
      "检测到多个 GWAS 汇总统计相关字段，例如 SNP、beta、SE 或 P 值。",
    "Detected a mostly numeric matrix-like object with many rows and fewer columns." =
      "检测到对象大多为数值型，且呈现“行多列少”的矩阵特征。",
    "Detected mixed clinical-style columns such as outcome, treatment, time, or group." =
      "检测到结局、治疗、时间或分组等临床表常见字段。",
    "Detected a numeric matrix with named columns." =
      "检测到带列名的数值矩阵。",
    "The object does not clearly match the first-version heuristics." =
      "现有规则下未识别出明确的数据类型。",
    reason
  )
}

.mednova_app_localize_error <- function(message) {
  if (!is.character(message) || length(message) != 1L || !nzchar(message)) {
    return(message)
  }

  translations <- c(
    "`task` must be a non-empty character string." = "`task` 必须是非空字符字符串。",
    "`data` must be provided." = "`data` 不能为空。",
    "`spec` must be a named list." = "`spec` 必须是命名 list。",
    "`data_name` must be a non-empty character string." = "`data_name` 必须是非空字符字符串。",
    "`save_to` must be a non-empty file path." = "`save_to` 必须是非空文件路径。",
    "Unsupported task description. Supported tasks are: propensity score matching, logistic regression, Cox regression, bulk RNA differential expression, Mendelian randomization, diagnostic modeling" =
      "暂不支持该任务描述。可选任务包括：propensity score matching、logistic regression、Cox regression、bulk RNA differential expression、Mendelian randomization、diagnostic modeling。"
  )

  if (message %in% names(translations)) {
    return(unname(translations[[message]]))
  }

  message
}

.mednova_app_function_note_cn <- function(current_row) {
  note_parts <- character()

  if ("input_mode" %in% names(current_row) && nzchar(current_row$input_mode)) {
    note_parts <- c(note_parts, paste0("输入模式：", current_row$input_mode, "。"))
  }

  if ("output_type" %in% names(current_row) && nzchar(current_row$output_type)) {
    note_parts <- c(note_parts, paste0("输出类型：", current_row$output_type, "。"))
  }

  if ("side_effect" %in% names(current_row) &&
      nzchar(current_row$side_effect) &&
      !identical(current_row$side_effect, "none")) {
    note_parts <- c(note_parts, paste0("潜在副作用：", current_row$side_effect, "。"))
  }

  paste(note_parts, collapse = " ")
}

.mednova_app_format_suggestions <- function(data_profile,
                                            matched_domain = NULL,
                                            selected_functions = NULL,
                                            column_mapping = list()) {
  mapping <- .mednova_app_compact_mapping(column_mapping)
  required_roles <- .mednova_app_expected_roles_for_domain(matched_domain)
  mapped_roles <- mapping[intersect(names(mapping), unique(c(required_roles, names(mapping))))]
  missing_roles <- required_roles[!vapply(
    required_roles,
    function(role) .mednova_app_has_value(mapping[[role]]),
    logical(1L)
  )]
  displayed_roles <- unique(c(required_roles, names(mapped_roles)))

  suggested_column_map <- if (length(displayed_roles)) {
    data.frame(
      canonical_role = displayed_roles,
      role_label = vapply(displayed_roles, .mednova_app_role_label, character(1L)),
      current_column = vapply(
        displayed_roles,
        function(role) {
          value <- mapping[[role]]

          if (!.mednova_app_has_value(value)) {
            return("")
          }

          paste(value, collapse = ", ")
        },
        character(1L)
      ),
      status = vapply(
        displayed_roles,
        function(role) {
          if (.mednova_app_has_value(mapping[[role]])) {
            return(if (role %in% required_roles) "已映射" else "可选")
          }

          "缺失"
        },
        character(1L)
      ),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      canonical_role = character(),
      role_label = character(),
      current_column = character(),
      status = character(),
      stringsAsFactors = FALSE
    )
  }

  notes <- c(
    paste0("当前数据更像 ", .mednova_app_structure_label_cn(data_profile$structure_guess), "。")
  )

  if (!is.null(data_profile$structure_reason) && nzchar(data_profile$structure_reason)) {
    notes <- c(notes, .mednova_app_translate_structure_reason(data_profile$structure_reason))
  }

  if (length(required_roles)) {
    notes <- c(
      notes,
      paste0(
        "当前任务最低需要：",
        paste(vapply(required_roles, .mednova_app_role_label, character(1L)), collapse = ", "),
        "。"
      )
    )
  }

  if (length(mapped_roles)) {
    notes <- c(
      notes,
      paste0(
        "当前已映射：",
        paste(
          vapply(
            names(mapped_roles),
            function(role) {
              paste0(.mednova_app_role_label(role), " -> ", paste(mapped_roles[[role]], collapse = ", "))
            },
            character(1L)
          ),
          collapse = "; "
        ),
        "。"
      )
    )
  }

  if (length(missing_roles)) {
    notes <- c(
      notes,
      paste0(
        "仍缺少：",
        paste(vapply(missing_roles, .mednova_app_role_label, character(1L)), collapse = ", "),
        "。"
      )
    )
  }

  mismatch_note <- .mednova_app_structure_note(
    structure_guess = data_profile$structure_guess,
    matched_domain = matched_domain
  )
  if (!is.null(mismatch_note)) {
    notes <- c(notes, mismatch_note)
  }

  if (is.data.frame(selected_functions) && nrow(selected_functions) > 0L) {
    notes <- c(
      notes,
      paste0(
        "当前优先推荐函数包括：",
        paste(utils::head(selected_functions$function_name, 3L), collapse = ", "),
        "。"
      )
    )
  }

  list(
    structure_guess = data_profile$structure_guess,
    required_roles = required_roles,
    mapped_roles = mapped_roles,
    missing_roles = missing_roles,
    suggested_column_map = suggested_column_map,
    notes = unique(notes)
  )
}

.mednova_app_requirement_mapping_key <- function(role) {
  switch(
    role,
    treatment = "treatment",
    exposure = "treatment",
    outcome = "outcome",
    event = "event",
    time = "time",
    sample_id = "sample_id",
    id = "sample_id",
    group = "group",
    predictors = "covariates",
    predictor = "covariates",
    covariates = "covariates",
    covariate = "covariates",
    marker = "covariates",
    feature = "covariates",
    features = "covariates",
    target_genes = "covariates",
    variable = "covariates",
    NULL
  )
}

.mednova_app_split_required_cols <- function(required_cols) {
  if (!is.character(required_cols) || length(required_cols) != 1L || !nzchar(required_cols)) {
    return(character())
  }

  trimws(strsplit(required_cols, ";", fixed = TRUE)[[1L]])
}

.mednova_app_function_requirements <- function(selected_functions,
                                               column_mapping = list()) {
  empty_table <- data.frame(
    function_name = character(),
    required_columns = character(),
    current_mapped_columns = character(),
    missing_required_columns = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )

  if (!is.data.frame(selected_functions) || !nrow(selected_functions)) {
    return(empty_table)
  }

  mapping <- .mednova_app_compact_mapping(column_mapping)
  rows <- lapply(seq_len(nrow(selected_functions)), function(i) {
    current_row <- selected_functions[i, , drop = FALSE]
    required_cols <- .mednova_app_split_required_cols(current_row$required_cols)
    mapped_bits <- character()
    missing_bits <- character()
    derived_bits <- character()

    for (required_col in required_cols) {
      mapping_key <- .mednova_app_requirement_mapping_key(required_col)

      if (is.null(mapping_key)) {
        derived_bits <- c(derived_bits, required_col)
        next
      }

      mapped_value <- mapping[[mapping_key]]

      if (.mednova_app_has_value(mapped_value)) {
        mapped_bits <- c(
          mapped_bits,
          paste0(required_col, ": ", paste(mapped_value, collapse = ", "))
        )
      } else {
        missing_bits <- c(missing_bits, required_col)
      }
    }

    note_parts <- character()

    if (length(derived_bits)) {
      note_parts <- c(
        note_parts,
        paste0(
          "还需要结构化或中间产物输入：",
          paste(derived_bits, collapse = ", "),
          "。"
        )
      )
    }

    note_parts <- c(note_parts, .mednova_app_function_note_cn(current_row))

    data.frame(
      function_name = current_row$function_name,
      required_columns = if (length(required_cols)) paste(required_cols, collapse = ", ") else "未列出",
      current_mapped_columns = if (length(mapped_bits)) paste(mapped_bits, collapse = "; ") else "当前尚无可用映射",
      missing_required_columns = if (length(missing_bits)) paste(missing_bits, collapse = ", ") else "无",
      notes = paste(note_parts, collapse = " "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

.mednova_app_task_configs <- function() {
  list(
    propensity_score_matching = list(
      task_label_cn = "倾向评分匹配",
      task_text = "propensity score matching",
      main_function = "med_psm()",
      required_inputs = c(
        treatment = "治疗 / 暴露列",
        covariates = "协变量 / 预测变量"
      ),
      optional_inputs = character(),
      steps = c(
        "读取治疗 / 暴露列和协变量。",
        "调用 med_psm() 执行 propensity score matching。",
        "输出 matched data 和 baseline tables。"
      )
    ),
    logistic_regression = list(
      task_label_cn = "Logistic 回归",
      task_text = "logistic regression",
      main_function = "med_logistic()",
      required_inputs = c(
        outcome = "结局列",
        covariates = "协变量 / 预测变量"
      ),
      optional_inputs = character(),
      steps = c(
        "读取结局列和预测变量。",
        "调用 med_logistic() 拟合模型。",
        "输出回归结果。"
      )
    ),
    cox_regression = list(
      task_label_cn = "Cox 回归",
      task_text = "Cox regression",
      main_function = "med_cox()",
      required_inputs = c(
        time = "时间列",
        event = "事件列",
        covariates = "协变量 / 预测变量"
      ),
      optional_inputs = character(),
      steps = c(
        "读取时间列、事件列和预测变量。",
        "调用 med_cox() 拟合 Cox 模型。",
        "输出风险比结果。"
      )
    ),
    bulk_rna_differential_expression = list(
      task_label_cn = "Bulk RNA 差异表达",
      task_text = "bulk RNA differential expression",
      main_function = "med_bulk_deg()",
      required_inputs = c(
        group = "分组列"
      ),
      optional_inputs = c(
        sample_id = "样本 ID 列"
      ),
      steps = c(
        "读取分组列。",
        "把其余表达列整理为表达矩阵。",
        "调用 med_bulk_deg() 进行差异分析。",
        "输出差异结果。"
      )
    ),
    mendelian_randomization = list(
      task_label_cn = "孟德尔随机化",
      task_text = "Mendelian randomization",
      main_function = "med_mr()",
      required_inputs = c(
        snp = "SNP 列",
        beta = "beta 列",
        se = "SE 列",
        eaf = "EAF 列",
        effect_allele = "effect allele 列",
        other_allele = "other allele 列",
        pval = "P 值列",
        n = "样本量列",
        chr = "染色体列",
        pos = "位置列",
        outcome_data_name = "Outcome 数据对象名"
      ),
      optional_inputs = c(
        exposure_name = "Exposure 名称"
      ),
      steps = c(
        "读取 GWAS 关键列。",
        "整理 exposure 数据和列映射。",
        "调用 med_mr() 运行 MR 流程。",
        "输出 MR 结果对象。"
      )
    ),
    diagnostic_modeling = list(
      task_label_cn = "诊断模型",
      task_text = "diagnostic modeling",
      main_function = "med_diagnostic_model()",
      required_inputs = c(
        outcome = "结局 / 分组列",
        covariates = "预测变量"
      ),
      optional_inputs = c(
        sample_id = "样本 ID 列"
      ),
      steps = c(
        "读取结局 / 分组列和预测变量。",
        "把样本表整理为 expr 和 group_df。",
        "调用 med_diagnostic_model() 建模。",
        "输出模型和 ROC 结果。"
      )
    )
  )
}

.mednova_app_task_config <- function(domain) {
  if (is.null(domain) || !nzchar(domain)) {
    return(NULL)
  }

  .mednova_app_task_configs()[[domain]]
}

.mednova_app_process_info <- function(domain) {
  config <- .mednova_app_task_config(domain)

  if (is.null(config)) {
    return(NULL)
  }

  list(
    task_label_cn = config$task_label_cn,
    main_function = config$main_function,
    steps = config$steps
  )
}

.mednova_app_value_text <- function(value) {
  if (!.mednova_app_has_value(value)) {
    return("")
  }

  if (length(value) > 1L) {
    return(paste(value, collapse = ", "))
  }

  as.character(value)
}

.mednova_app_input_requirements_simple <- function(domain, input_values) {
  config <- .mednova_app_task_config(domain)

  if (is.null(config)) {
    return(NULL)
  }

  required_inputs <- config$required_inputs
  optional_inputs <- config$optional_inputs

  build_rows <- function(items, required = TRUE) {
    if (!length(items)) {
      return(NULL)
    }

    rows <- lapply(seq_along(items), function(i) {
      key <- names(items)[i]
      label <- unname(items[[i]])
      current_value <- .mednova_app_value_text(input_values[[key]])
      has_value <- nzchar(current_value)

      data.frame(
        输入项 = label,
        当前选择 = if (has_value) current_value else "未设置",
        状态 = if (has_value) "已选择" else if (required) "缺少" else "可选",
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, rows)
  }

  required_table <- build_rows(required_inputs, required = TRUE)
  optional_table <- build_rows(optional_inputs, required = FALSE)
  table <- rbind(required_table, optional_table)
  missing_labels <- unname(required_inputs[!vapply(
    names(required_inputs),
    function(key) .mednova_app_has_value(input_values[[key]]),
    logical(1L)
  )])

  list(
    main_function = config$main_function,
    missing_inputs = missing_labels,
    is_complete = !length(missing_labels),
    status_text = if (!length(missing_labels)) {
      "输入完整，可生成代码"
    } else {
      paste0("仍缺少：", paste(missing_labels, collapse = "、"))
    },
    table = table
  )
}

.mednova_app_build_generation_spec <- function(domain, input_values) {
  spec <- switch(
    domain,
    propensity_score_matching = list(
      treatment = input_values$treatment,
      exposure = input_values$treatment,
      covariates = input_values$covariates
    ),
    logistic_regression = list(
      outcome = input_values$outcome,
      covariates = input_values$covariates
    ),
    cox_regression = list(
      time = input_values$time,
      event = input_values$event,
      covariates = input_values$covariates
    ),
    bulk_rna_differential_expression = list(
      group = input_values$group,
      sample_id = input_values$sample_id
    ),
    mendelian_randomization = list(
      snp = input_values$snp,
      beta = input_values$beta,
      se = input_values$se,
      eaf = input_values$eaf,
      effect_allele = input_values$effect_allele,
      other_allele = input_values$other_allele,
      pval = input_values$pval,
      n = input_values$n,
      chr = input_values$chr,
      pos = input_values$pos,
      outcome_data_name = input_values$outcome_data_name,
      exposure_name = input_values$exposure_name
    ),
    diagnostic_modeling = list(
      outcome = input_values$outcome,
      covariates = input_values$covariates,
      sample_id = input_values$sample_id
    ),
    list()
  )

  spec[vapply(spec, .mednova_app_has_value, logical(1L))]
}

.mednova_app_task_object <- function(domain) {
  config <- .mednova_app_task_config(domain)

  if (is.null(config)) {
    return(NULL)
  }

  list(
    domain = domain,
    task_label = config$task_text
  )
}

.mednova_app_studio_ui <- function() {
  shiny::div(
    class = "platform-page platform-page--studio",
    shiny::div(
      class = "studio-header studio-header--platform",
      shiny::div(
        class = "studio-header__text",
        shiny::tags$span(class = "platform-kicker", "Studio 工作台"),
        shiny::tags$h1("MedNova Studio"),
        shiny::tags$p("基于任务配置和用户选择生成 R 代码")
      )
    ),
    shiny::div(
      class = "studio-main",
      shiny::div(
        class = "studio-top-grid",
        shiny::div(class = "studio-top-grid__input", mod_upload_ui("upload")),
        shiny::div(class = "studio-top-grid__code", mod_code_ui("code"))
      ),
      shiny::div(
        class = "studio-card studio-card--info",
        shiny::div(
          class = "studio-info-header",
          shiny::tags$h2("信息区"),
          shiny::tags$p("输入数据、函数流程和输入要求。")
        ),
        shiny::tabsetPanel(
          id = "information_tabs",
          type = "pills",
          selected = "输入数据信息",
          shiny::tabPanel(
            "输入数据信息",
            mod_profile_ui("profile")
          ),
          shiny::tabPanel(
            "函数处理流程",
            mod_recommend_ui("recommend")
          ),
          shiny::tabPanel(
            "函数输入要求",
            mod_requirements_ui("requirements")
          )
        )
      )
    )
  )
}

ui <- shiny::tagList(
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    shiny::tags$script(shiny::HTML(
      "
      Shiny.addCustomMessageHandler('mednova-copy-script', function(message) {
        var text = message.text || '';
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text);
          return;
        }
        var el = document.createElement('textarea');
        el.value = text;
        document.body.appendChild(el);
        el.select();
        document.execCommand('copy');
        document.body.removeChild(el);
      });

      Shiny.addCustomMessageHandler('mednova-clear-file', function(message) {
        var input = document.getElementById(message.id);
        if (input) {
          input.value = '';
        }
      });
      "
    ))
  ),
  shiny::navbarPage(
    title = shiny::tags$span(class = "platform-nav-brand", "MedNova"),
    id = "platform_nav",
    collapsible = TRUE,
    theme = bslib::bs_theme(
      version = 5,
      primary = "#0f7a87",
      secondary = "#6b8fa1",
      bg = "#f3f9fb",
      fg = "#18354a",
      base_font = bslib::font_google("IBM Plex Sans"),
      code_font = bslib::font_google("IBM Plex Mono")
    ),
    windowTitle = "MedNova",
    shiny::tabPanel("首页", value = "home", mod_home_ui("home")),
    shiny::tabPanel("平台介绍", value = "about", mod_about_ui("about")),
    shiny::tabPanel("Studio 工作台", value = "studio", .mednova_app_studio_ui()),
    shiny::tabPanel("文档中心", value = "docs", mod_docs_ui("docs")),
    shiny::tabPanel("教程案例", value = "cases", mod_cases_ui("cases")),
    shiny::tabPanel("FAQ", value = "faq", mod_faq_ui("faq"))
  )
)

server <- function(input, output, session) {
  preview_data <- shiny::reactiveVal(NULL)
  preview_profile <- shiny::reactiveVal(NULL)
  analysis_result <- shiny::reactiveVal(NULL)
  error_message <- shiny::reactiveVal(NULL)

  home_nav <- mod_home_server("home")
  about_nav <- mod_about_server("about")
  docs_nav <- mod_docs_server("docs")
  cases_nav <- mod_cases_server("cases")
  faq_nav <- mod_faq_server("faq")

  shiny::observeEvent(home_nav$go_about(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "about")
  }, ignoreInit = TRUE)

  shiny::observeEvent(home_nav$go_studio(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "studio")
  }, ignoreInit = TRUE)

  shiny::observeEvent(home_nav$go_docs(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "docs")
  }, ignoreInit = TRUE)

  shiny::observeEvent(home_nav$go_cases(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "cases")
  }, ignoreInit = TRUE)

  shiny::observeEvent(about_nav$go_studio(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "studio")
  }, ignoreInit = TRUE)

  shiny::observeEvent(about_nav$go_docs(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "docs")
  }, ignoreInit = TRUE)

  shiny::observeEvent(about_nav$go_cases(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "cases")
  }, ignoreInit = TRUE)

  shiny::observeEvent(docs_nav$go_studio(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "studio")
  }, ignoreInit = TRUE)

  shiny::observeEvent(docs_nav$go_cases(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "cases")
  }, ignoreInit = TRUE)

  shiny::observeEvent(cases_nav$go_studio(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "studio")
  }, ignoreInit = TRUE)

  shiny::observeEvent(cases_nav$go_docs(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "docs")
  }, ignoreInit = TRUE)

  shiny::observeEvent(faq_nav$go_docs(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "docs")
  }, ignoreInit = TRUE)

  shiny::observeEvent(faq_nav$go_studio(), {
    shiny::updateNavbarPage(session, "platform_nav", selected = "studio")
  }, ignoreInit = TRUE)

  upload_state <- mod_upload_server(
    "upload",
    column_names = shiny::reactive({
      profile <- preview_profile()
      if (is.null(profile)) character() else profile$column_names %||% character()
    })
  )

  process_info <- shiny::reactive({
    .mednova_app_process_info(upload_state$task_domain())
  })

  requirements_info <- shiny::reactive({
    .mednova_app_input_requirements_simple(
      domain = upload_state$task_domain(),
      input_values = upload_state$input_values()
    )
  })

  shiny::observeEvent(
    list(upload_state$file(), upload_state$data_name()),
    {
      file_info <- upload_state$file()

      if (is.null(file_info)) {
        preview_data(NULL)
        preview_profile(NULL)
        return()
      }

      uploaded_data <- tryCatch(
        .mednova_app_read_data(file_info),
        error = function(e) {
          error_message(.mednova_app_localize_error(conditionMessage(e)))
          NULL
        }
      )

      if (is.null(uploaded_data)) {
        preview_data(NULL)
        preview_profile(NULL)
        return()
      }

      error_message(NULL)
      preview_data(uploaded_data)
      preview_profile(
        .mednova_app_exported("mednova_inspect_data")(
          uploaded_data,
          data_name = upload_state$data_name()
        )
      )
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(upload_state$reset(), {
    preview_data(NULL)
    preview_profile(NULL)
    analysis_result(NULL)
    error_message(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(upload_state$generate(), {
    analysis_result(NULL)
    error_message(NULL)

    domain <- upload_state$task_domain()
    if (!nzchar(domain)) {
      error_message("请先选择任务类型。")
      return()
    }

    uploaded_data <- preview_data()
    if (is.null(uploaded_data)) {
      uploaded_data <- tryCatch(
        .mednova_app_read_data(upload_state$file()),
        error = function(e) {
          error_message(.mednova_app_localize_error(conditionMessage(e)))
          NULL
        }
      )
    }
    if (is.null(uploaded_data)) {
      return()
    }

    requirement_obj <- requirements_info()
    if (!is.null(requirement_obj) && !isTRUE(requirement_obj$is_complete)) {
      error_message(requirement_obj$status_text)
      return()
    }

    data_profile <- .mednova_app_exported("mednova_inspect_data")(
      uploaded_data,
      data_name = upload_state$data_name()
    )
    task_obj <- .mednova_app_task_object(domain)
    spec <- .mednova_app_build_generation_spec(domain, upload_state$input_values())

    script <- tryCatch(
      .mednova_app_exported("mednova_generate_script")(
        task = task_obj,
        data = uploaded_data,
        spec = spec,
        data_profile = data_profile,
        assumptions = character()
      ),
      error = function(e) {
        error_message(.mednova_app_localize_error(conditionMessage(e)))
        NULL
      }
    )

    if (is.null(script)) {
      return()
    }

    script <- .mednova_app_attach_task_note(script, upload_state$task_note())

    analysis_result(
      structure(
        list(
          data_profile = data_profile,
          matched_domain = domain,
          selected_functions = .mednova_app_exported("mednova_plan_workflow")(task_obj)$selected_functions,
          workflow_steps = process_info()$steps %||% character(),
          assumptions = character(),
          script = script
        ),
        class = "mednova_result"
      )
    )
  }, ignoreInit = TRUE)

  mod_profile_server(
    "profile",
    profile = shiny::reactive(preview_profile()),
    preview_data = shiny::reactive(preview_data()),
    error = shiny::reactive(error_message())
  )

  mod_recommend_server(
    "recommend",
    process_info = shiny::reactive(process_info()),
    error = shiny::reactive(error_message())
  )

  mod_requirements_server(
    "requirements",
    requirements = shiny::reactive(requirements_info()),
    error = shiny::reactive(error_message())
  )

  mod_code_server(
    "code",
    result = shiny::reactive(analysis_result()),
    error = shiny::reactive(error_message())
  )
}

shiny::shinyApp(ui = ui, server = server)
