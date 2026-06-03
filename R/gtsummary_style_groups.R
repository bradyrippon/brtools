
#' Style `gtsummary` variable group headers
#'
#' @param tbl A `gtsummary` table.
#' @param indent Indentation size for level rows.
#'
#' @return A  styled `gtsummary` table.
#'
#' @export
gtsummary_style_groups <- function(tbl, indent = 8L){

  if (!inherits(tbl, "gtsummary")) {
    cli::cli_abort(
      c(
        "!" = "{.arg tbl} must be a {.cls gtsummary} object.",
        "i" = "Current class: {.cls {class(tbl)[1]}}"
      )
    )
  }

  tbl |>
    gtsummary::modify_indent(
      columns = "label",
      rows    = row_type == "level",
      indent  = indent
    ) |>
    gtsummary::modify_table_styling(
      columns     = label,
      rows        = row_type == "variable_group",
      text_format = "bold"
    )

}
