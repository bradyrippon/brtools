
#' Add download buttons for a gtsummary table in an HTML file
#'
#' @param tbl A gtsummary table.
#' @param name A single non-empty string used as the file name.
#' @param dir Directory where exported files should be saved.
#' @param label Button label.
#' @param color_border Button border color.
#' @param color_bg Button background color.
#' @param color_text Button text color.
#' @param color_option_border Option border color.
#' @param color_option_bg Option background color.
#' @param color_option_text Option text color.
#' @param color_option_hover_bg Option hover background color.
#' @param color_option_hover_text Option hover text color.
#'
#' @return An HTML tag list containing the download button. Pastes raw HTML into console if used outside of markdown render.
#'
#' @export
tbl_button <- function(
    tbl,
    name,
    dir = here::here("reports", "downloads"),
    label = "Download Table",

    color_border            = "#6c757d",
    color_bg                = "#f8f9fa",
    color_text              = "inherit",
    color_option_border     = "#ced4da",
    color_option_bg         = "white",
    color_option_text       = "inherit",
    color_option_hover_bg   = "#e9ecef",
    color_option_hover_text = "inherit"
) {

  if (missing(name) || !is.character(name) || length(name) != 1 || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }

  if (!inherits(tbl, "gtsummary")) {
    cli::cli_abort(
      c(
        "!" = "{.arg tbl} must be a {.cls gtsummary} object.",
        "i" = "Current class: {.cls {class(tbl)[1]}}"
      )
    )
  }

  css_path <- system.file("tbl_button.css", package = "brtools")

  if (!dir.exists(dir)) {
    cli::cli_alert_info("Creating {.file {dir}/} directory.")
    dir.create(dir, recursive = TRUE)
  }

  docx_path <- file.path(dir, paste0(name, ".docx"))
  xlsx_path <- file.path(dir, paste0(name, ".xlsx"))

  clean_md <- function(x) {
    stringr::str_remove_all(x, "\\*\\*|__")
  }

  tryCatch(
    {
      tbl |>
        gtsummary::as_flex_table() |>
        flextable::save_as_docx(path = docx_path)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "!" = "Failed to create Word document from {.cls gtsummary} table.",
          "x" = e$message
        )
      )
    }
  )

  xlsx_tbl <-
    tbl |>
    gtsummary::as_tibble() |>
    dplyr::rename_with(clean_md) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ clean_md(as.character(.x))
      )
    )

  tryCatch(
    {
      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, name)

      openxlsx::writeData(
        wb,
        sheet = name,
        x = xlsx_tbl
      )

      header_style <- openxlsx::createStyle(
        textDecoration = "bold"
      )

      openxlsx::addStyle(
        wb,
        sheet = name,
        style = header_style,
        rows = 1,
        cols = seq_len(ncol(xlsx_tbl)),
        gridExpand = TRUE
      )

      openxlsx::freezePane(
        wb,
        sheet = name,
        firstRow = TRUE
      )

      openxlsx::setColWidths(
        wb,
        sheet = name,
        cols = seq_len(ncol(xlsx_tbl)),
        widths = "auto"
      )

      openxlsx::saveWorkbook(
        wb,
        file = xlsx_path,
        overwrite = TRUE
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "!" = "Failed to create Excel workbook.",
          "x" = e$message
        )
      )
    }
  )

  css <- paste(
    readLines(css_path, warn = FALSE),
    collapse = "\n"
  )

  button_id <- gsub("[^A-Za-z0-9_-]", "-", name)

  button_vars <- glue::glue(
"
.tbl-button-{button_id} {{
  --border: {color_border};
  --bg: {color_bg};
  --text: {color_text};

  --option-border: {color_option_border};
  --option-bg: {color_option_bg};
  --option-text: {color_option_text};

  --option-hover-bg: {color_option_hover_bg};
  --option-hover-text: {color_option_hover_text};
}}
"
  )

  cli::cli_alert_success(
    "Created {.file {basename(docx_path)}} and {.file {basename(xlsx_path)}}."
  )

  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(css)),
    htmltools::tags$style(htmltools::HTML(button_vars)),

    htmltools::tags$div(
      class = "tbl-button-wrap",

      htmltools::tags$div(
        class = paste("tbl-button", paste0("tbl-button-", button_id)),

        htmltools::tags$span(
          class = "tbl-button-label",
          label
        ),

        htmltools::tags$a(
          class = "tbl-button-option",
          href = xlsx_path,
          download = NA,
          ".xlsx"
        ),

        htmltools::tags$a(
          class = "tbl-button-option",
          href = docx_path,
          download = NA,
          ".docx"
        )
      )
    )
  )
}
