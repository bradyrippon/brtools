
#' Color `gtsummary` variable group headers
#'
#' @param tbl A `gtsummary` table.
#' @param color Text color for variable group rows.
#' @param fill Fill color for variable group rows.
#'
#' @return A styled `gt` table.
#'
#' @export
gt_color_groups <- function(
    tbl,
    color = "#B31B1B",
    fill  = "grey95"
){

  if (!inherits(tbl, "gtsummary")) {
    cli::cli_abort(
      c(
        "!" = "{.arg tbl} must be a {.cls gtsummary} object.",
        "i" = "Current class: {.cls {class(tbl)[1]}}"
      )
    )
  }

  tbl |>
    gtsummary::as_gt() |>
    gt::tab_style(
      style = list(
        gt::cell_text(color = color),
        gt::cell_fill(color = fill)
      ),
      locations = gt::cells_body(
        rows = row_type == "variable_group"
      )
    )

}
