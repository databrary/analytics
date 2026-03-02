#' Update Data About Institutions and Investigators
#' 
#' @param csv_dir A character string. The directory to store the data.
#' @param csv_fn A character string. The name of the saved file.
#' 
update_inst_invest_df <-
  function(csv_dir = "src/csv", csv_fn = "institutions-investigators.csv", 
           rq = NULL) {
    stopifnot(is.character(csv_dir))
    stopifnot(is.character(csv_fn))
    
    old_df <- load_old_inst_invest_data(csv_dir, csv_fn)
    new_item <- get_inst_invest_datab(rq)
    
    new_df <- old_df
    next_entry <- dim(old_df)[1] + 1
    new_df[next_entry, ] = NA
    new_df[next_entry, ] <- new_item
    new_df
  }

#-------------------------------------------------------------------------------
load_old_inst_invest_data <-
  function(csv_dir = "src/csv", csv_fn = "institutions-investigators.csv") {
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    old_stats <-
      readr::read_csv(file.path(csv_dir, csv_fn), show_col_types = FALSE)
    dplyr::mutate(old_stats, date = lubridate::as_datetime(date))
  }

#-------------------------------------------------------------------------------
get_inst_invest_datab <- function(rq = NULL) {

  new_stats <- databraryr::get_db_stats(rq = rq)
  new_stats$date <- lubridate::as_datetime(new_stats$date)
  
  new_stats <- new_stats |>
    dplyr::select(date, institutions, investigators, affiliates) |>
    dplyr::mutate(date = lubridate::as_datetime(date))
  
  if (rlang::is_empty(new_stats)) {
    warning("Unable to retrieve new statistics from Databrary.")
    NULL
  } else {
    new_stats
  }
}
