
#' Start a Toggl timer for coding time
#'
#' @param desc Timer description. Defaults to the active project name.
#' @param tags Toggl tag. Defaults to the parent folder name.
#'
#' @export
toggl_code <-
  function(
    desc = "Code/Reports",
    tags = here::here() |> dirname() |> basename()
  ) {
    timer <- .prepare_toggl(desc, tags)

    togglr::toggl_start(
      description = timer$desc,
      tags        = timer$tags
    )
    cli::cli_alert_success("Started timer with tag {.val {timer$tags}}.")
  }


#' Start a Toggl timer for research time
#'
#' @param desc Timer description. Defaults to the active project name.
#' @param tags Toggl tag. Defaults to the parent folder name.
#'
#' @export
toggl_research <-
  function(
    desc = "Research/Writing",
    tags = here::here() |> dirname() |> basename()
  ) {
    timer <- .prepare_toggl(desc, tags)

    togglr::toggl_start(
      description = timer$desc,
      tags        = timer$tags
    )
    cli::cli_alert_success("Started timer with tag {.val {timer$tags}}.")
  }


#' Start a Toggl timer for research work
#'
#' @param desc Timer description. Defaults to the active project name.
#' @param tags Toggl tag. Defaults to the parent folder name.
#'
#' @export
toggl_meeting <-
  function(
    desc = "Meetings/Emails",
    tags = here::here() |> dirname() |> basename()
  ) {
    timer <- .prepare_toggl(desc, tags)

    togglr::toggl_start(
      description = timer$desc,
      tags        = timer$tags
    )
    cli::cli_alert_success("Started timer with tag {.val {timer$tags}}.")
  }



## helpers ----------

.prepare_toggl <- function(desc, tags) {

  project <- rstudioapi::getActiveProject()
  if (is.null(project)) {cli::cli_abort("No active project found.")}

  cli::cli_alert_info("Suggested tag: {.val {tags}}")
  confirm <- readline("Use this tag? [Y/N]: ")

  if (toupper(confirm) == "N") {tags <- readline("Enter tag: ")}

  list(
    desc = desc,
    tags = tags
  )
}
