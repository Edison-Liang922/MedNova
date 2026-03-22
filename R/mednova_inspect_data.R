#' 检查数据对象的轻量元信息
#'
#' @param data 用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。
#' @param data_name 可选对象名，用于生成脚本时引用。
#'
#' @return 返回一个 list，包含对象类型、维度、列名、数值列、字符列、
#'   启发式列角色猜测以及粗略结构判断。
#' @export
mednova_inspect_data <- function(data, data_name = NULL) {
  if (is.null(data_name)) {
    data_name <- .mednova_resolve_data_name(data, substitute(data))
  }

  if (is.character(data) && length(data) == 1L) {
    return(
      list(
        object_name = data_name,
        object_class = "symbolic_reference",
        object_type = "symbolic_reference",
        dimensions = list(rows = NA_integer_, cols = NA_integer_),
        column_names = character(),
        numeric_columns = character(),
        character_columns = character(),
        guessed_columns = list(),
        structure_guess = "unknown",
        structure_reason = "Only an object name was supplied, so the structure could not be inspected."
      )
    )
  }

  object_type <- .mednova_detect_object_type(data)
  column_names <- .mednova_get_column_names(data)
  numeric_columns <- .mednova_get_numeric_columns(data)
  character_columns <- .mednova_get_character_columns(data)
  guessed_columns <- .mednova_guess_columns(column_names)
  structure_guess <- .mednova_guess_structure(
    data = data,
    object_type = object_type,
    column_names = column_names,
    numeric_columns = numeric_columns,
    character_columns = character_columns,
    guessed_columns = guessed_columns
  )

  list(
    object_name = data_name,
    object_class = class(data),
    object_type = object_type,
    dimensions = .mednova_get_dimensions(data),
    column_names = column_names,
    numeric_columns = numeric_columns,
    character_columns = character_columns,
    guessed_columns = guessed_columns,
    structure_guess = structure_guess$type,
    structure_reason = structure_guess$reason
  )
}

.mednova_detect_object_type <- function(data) {
  if (is.data.frame(data)) {
    return("data.frame")
  }

  if (is.matrix(data)) {
    return("matrix")
  }

  if (is.list(data)) {
    return("list")
  }

  "object"
}

.mednova_get_dimensions <- function(data) {
  if (is.data.frame(data) || is.matrix(data)) {
    dims <- dim(data)
    return(list(rows = dims[1L], cols = dims[2L]))
  }

  if (is.list(data)) {
    return(list(rows = length(data), cols = NA_integer_))
  }

  list(rows = length(data), cols = NA_integer_)
}

.mednova_get_column_names <- function(data) {
  if (is.data.frame(data) || is.matrix(data)) {
    return(colnames(data) %||% character())
  }

  if (is.list(data)) {
    return(names(data) %||% character())
  }

  character()
}

.mednova_get_numeric_columns <- function(data) {
  if (is.data.frame(data)) {
    keep <- vapply(data, is.numeric, logical(1L))
    return(names(data)[keep])
  }

  if (is.matrix(data) && is.numeric(data)) {
    return(colnames(data) %||% character())
  }

  if (is.list(data) && length(data)) {
    keep <- vapply(data, .mednova_component_has_numeric, logical(1L))
    return((names(data) %||% character())[keep])
  }

  character()
}

.mednova_get_character_columns <- function(data) {
  if (is.data.frame(data)) {
    keep <- vapply(
      data,
      function(x) is.character(x) || is.factor(x),
      logical(1L)
    )
    return(names(data)[keep])
  }

  if (is.matrix(data) && is.character(data)) {
    return(colnames(data) %||% character())
  }

  if (is.list(data) && length(data)) {
    keep <- vapply(data, .mednova_component_has_character, logical(1L))
    return((names(data) %||% character())[keep])
  }

  character()
}

.mednova_component_has_numeric <- function(x) {
  if (is.data.frame(x)) {
    return(any(vapply(x, is.numeric, logical(1L))))
  }

  if (is.matrix(x)) {
    return(is.numeric(x))
  }

  is.numeric(x)
}

.mednova_component_has_character <- function(x) {
  if (is.data.frame(x)) {
    return(any(vapply(
      x,
      function(col) is.character(col) || is.factor(col),
      logical(1L)
    )))
  }

  if (is.matrix(x)) {
    return(is.character(x))
  }

  is.character(x) || is.factor(x)
}

.mednova_guess_columns <- function(column_names) {
  if (!length(column_names)) {
    return(list())
  }

  alias_catalog <- .mednova_read_csv_resource("catalog", "column_aliases.csv")
  normalized_columns <- .mednova_normalize_text(column_names)
  guesses <- list()

  for (role in unique(alias_catalog$canonical_role)) {
    aliases <- alias_catalog$alias[alias_catalog$canonical_role == role]
    normalized_aliases <- .mednova_normalize_text(aliases)

    exact_match <- match(normalized_aliases, normalized_columns, nomatch = 0L)
    exact_match <- exact_match[exact_match > 0L]

    if (length(exact_match)) {
      guesses[[role]] <- column_names[exact_match[1L]]
      next
    }

    partial_match <- vapply(
      normalized_columns,
      function(column_name) any(vapply(
        normalized_aliases,
        function(alias_name) {
          grepl(column_name, alias_name, fixed = TRUE) ||
            grepl(alias_name, column_name, fixed = TRUE)
        },
        logical(1L)
      )),
      logical(1L)
    )

    if (any(partial_match)) {
      guesses[[role]] <- column_names[which(partial_match)[1L]]
    }
  }

  guesses
}

.mednova_guess_structure <- function(data,
                                     object_type,
                                     column_names,
                                     numeric_columns,
                                     character_columns,
                                     guessed_columns) {
  component_names <- if (is.list(data)) names(data) %||% character() else character()
  guessed_roles <- names(guessed_columns)
  gwas_roles <- c(
    "snp", "beta", "se", "pval", "chr", "pos",
    "effect_allele", "other_allele", "eaf"
  )
  clinical_roles <- c(
    "outcome", "event", "time", "treatment", "group", "case_control"
  )

  if (object_type == "list" && all(c("counts", "col_data") %in% component_names)) {
    return(list(
      type = "expression_data",
      reason = "Detected a list with `counts` and `col_data` components."
    ))
  }

  if (object_type == "list" &&
      all(c("exposure_dat", "outcome_dat") %in% component_names)) {
    return(list(
      type = "gwas_summary",
      reason = "Detected a list with `exposure_dat` and `outcome_dat` components."
    ))
  }

  if (length(intersect(guessed_roles, gwas_roles)) >= 4L) {
    return(list(
      type = "gwas_summary",
      reason = "Detected several GWAS summary statistic columns such as SNP, beta, SE, or p-value."
    ))
  }

  if (.mednova_looks_like_expression_matrix(data, object_type, numeric_columns)) {
    return(list(
      type = "expression_data",
      reason = "Detected a mostly numeric matrix-like object with many rows and fewer columns."
    ))
  }

  if (object_type == "data.frame" &&
      (length(intersect(guessed_roles, clinical_roles)) >= 2L ||
        (length(numeric_columns) >= 1L && length(character_columns) >= 1L))) {
    return(list(
      type = "clinical_table",
      reason = "Detected mixed clinical-style columns such as outcome, treatment, time, or group."
    ))
  }

  if (object_type == "matrix" && length(column_names) && !length(character_columns)) {
    return(list(
      type = "expression_data",
      reason = "Detected a numeric matrix with named columns."
    ))
  }

  list(
    type = "unknown",
    reason = "The object does not clearly match the first-version heuristics."
  )
}

.mednova_looks_like_expression_matrix <- function(data, object_type, numeric_columns) {
  if (!object_type %in% c("data.frame", "matrix")) {
    return(FALSE)
  }

  dims <- dim(data)
  has_row_names <- !is.null(rownames(data)) && length(rownames(data)) == dims[1L]
  mostly_numeric <- is.matrix(data) && is.numeric(data)

  if (is.data.frame(data) && dims[2L] > 0L) {
    mostly_numeric <- length(numeric_columns) >= max(1L, ceiling(dims[2L] * 0.8))
  }

  mostly_numeric &&
    has_row_names &&
    dims[1L] >= 20L &&
    dims[1L] > dims[2L]
}

.mednova_normalize_text <- function(x) {
  tolower(gsub("[^a-z0-9]+", "", x))
}

.mednova_find_package_root <- function(start = getwd(), max_up = 6L) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  for (i in seq_len(max_up + 1L)) {
    if (file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }

    parent <- dirname(current)

    if (identical(parent, current)) {
      break
    }

    current <- parent
  }

  NULL
}

.mednova_resource_path <- function(...) {
  installed_path <- system.file(..., package = "MedNova")

  if (nzchar(installed_path)) {
    return(installed_path)
  }

  package_root <- .mednova_find_package_root()

  if (!is.null(package_root)) {
    dev_path <- file.path(package_root, "inst", ...)

    if (file.exists(dev_path)) {
      return(dev_path)
    }
  }

  stop("Could not locate package resource: ", file.path(...), call. = FALSE)
}

.mednova_read_csv_resource <- function(...) {
  utils::read.csv(
    .mednova_resource_path(...),
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || !length(x)) {
    return(y)
  }

  x
}
