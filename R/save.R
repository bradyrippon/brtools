
#' Save a dataframe as an .rds file
#'
#' @param data Dataframe.
#' @param file File name for saved object (combined with current date).
#' @param loc Destination in R project. Defaults to `data` folder.
#'
#' @export
save_data <- function(data, file, loc = "data"){
  saveRDS(
    object = data,
    file   = here::here(loc, .as_today(file))
  )
}

#' Save a table as an .rds file
#'
#' @param data Dataframe.
#' @param file File name for saved object (combined with current date).
#' @param loc Destination in R project. Defaults to `reports/tbls` folder.
#'
#' @export
save_tbl <- function(data, file, loc = "reports/tbls"){
  saveRDS(
    object = data,
    file   = here::here(loc, .as_today(file))
  )
}



## helpers ----------

.as_today <- function(file){
  paste(
    paste0("[", Sys.Date(), "]"),
    file
  )
}
