.mednova_require_app_packages <- function() {
  required_packages <- c("shiny", "bslib", "DT")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "运行 MedNova Studio 需要安装 `", pkg, "` 包。",
        call. = FALSE
      )
    }
  }
}

.mednova_platform_theme <- function() {
  bslib::bs_theme(
    version = 5,
    primary = "#0f7a87",
    secondary = "#6b8fa1",
    bg = "#f3f9fb",
    fg = "#18354a",
    base_font = bslib::font_google("IBM Plex Sans"),
    code_font = bslib::font_google("IBM Plex Mono")
  )
}

.mednova_studio_head <- function(css_prefix = "mednova-studio-www") {
  shiny::tags$head(
    shiny::tags$title("MedNova Studio"),
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = paste0(css_prefix, "/styles.css")
    ),
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
  )
}

.mednova_register_studio_resource <- function(prefix = "mednova-studio-www") {
  paths <- shiny::resourcePaths()

  if (!prefix %in% names(paths)) {
    shiny::addResourcePath(prefix, .mednova_resource_path("app", "www"))
  }

  invisible(prefix)
}

.mednova_load_studio_env <- function() {
  app_dir <- .mednova_resource_path("app")
  app_env <- new.env(parent = baseenv())
  old_wd <- getwd()

  setwd(app_dir)
  on.exit(setwd(old_wd), add = TRUE)

  sys.source(file.path(app_dir, "app.R"), envir = app_env)
  app_env
}

.mednova_studio_server <- function(app_env) {
  function(input, output, session) {
    preview_data <- shiny::reactiveVal(NULL)
    preview_profile <- shiny::reactiveVal(NULL)
    analysis_result <- shiny::reactiveVal(NULL)
    error_message <- shiny::reactiveVal(NULL)

    upload_state <- app_env$mod_upload_server(
      "upload",
      column_names = shiny::reactive({
        profile <- preview_profile()
        if (is.null(profile)) character() else profile$column_names %||% character()
      })
    )

    process_info <- shiny::reactive({
      app_env$.mednova_app_process_info(upload_state$task_domain())
    })

    requirements_info <- shiny::reactive({
      app_env$.mednova_app_input_requirements_simple(
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
          app_env$.mednova_app_read_data(file_info),
          error = function(e) {
            error_message(app_env$.mednova_app_localize_error(conditionMessage(e)))
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
          app_env$.mednova_app_exported("mednova_inspect_data")(
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
          app_env$.mednova_app_read_data(upload_state$file()),
          error = function(e) {
            error_message(app_env$.mednova_app_localize_error(conditionMessage(e)))
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

      data_profile <- app_env$.mednova_app_exported("mednova_inspect_data")(
        uploaded_data,
        data_name = upload_state$data_name()
      )
      task_obj <- app_env$.mednova_app_task_object(domain)
      spec <- app_env$.mednova_app_build_generation_spec(
        domain,
        upload_state$input_values()
      )

      script <- tryCatch(
        app_env$.mednova_app_exported("mednova_generate_script")(
          task = task_obj,
          data = uploaded_data,
          spec = spec,
          data_profile = data_profile,
          assumptions = character()
        ),
        error = function(e) {
          error_message(app_env$.mednova_app_localize_error(conditionMessage(e)))
          NULL
        }
      )

      if (is.null(script)) {
        return()
      }

      script <- app_env$.mednova_app_attach_task_note(
        script,
        upload_state$task_note()
      )

      analysis_result(
        structure(
          list(
            data_profile = data_profile,
            matched_domain = domain,
            selected_functions = app_env$.mednova_app_exported("mednova_plan_workflow")(task_obj)$selected_functions,
            workflow_steps = process_info()$steps %||% character(),
            assumptions = character(),
            script = script
          ),
          class = "mednova_result"
        )
      )
    }, ignoreInit = TRUE)

    app_env$mod_profile_server(
      "profile",
      profile = shiny::reactive(preview_profile()),
      preview_data = shiny::reactive(preview_data()),
      error = shiny::reactive(error_message())
    )

    app_env$mod_recommend_server(
      "recommend",
      process_info = shiny::reactive(process_info()),
      error = shiny::reactive(error_message())
    )

    app_env$mod_requirements_server(
      "requirements",
      requirements = shiny::reactive(requirements_info()),
      error = shiny::reactive(error_message())
    )

    app_env$mod_code_server(
      "code",
      result = shiny::reactive(analysis_result()),
      error = shiny::reactive(error_message())
    )
  }
}

.mednova_studio_app <- function() {
  .mednova_require_app_packages()

  css_prefix <- .mednova_register_studio_resource()
  app_env <- .mednova_load_studio_env()

  shiny::shinyApp(
    ui = shiny::fluidPage(
      theme = .mednova_platform_theme(),
      .mednova_studio_head(css_prefix = css_prefix),
      app_env$.mednova_app_studio_ui()
    ),
    server = .mednova_studio_server(app_env)
  )
}
