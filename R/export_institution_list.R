export_institution_list <- function(file_path = "private/inst_list",
                                    vb = FALSE) {
  assertthat::assert_that(length(file_path) == 1)
  assertthat::is.string(file_path)
  assertthat::is.writeable(file_path)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  if (vb) message("Retrieving institution list from Databrary.")
  institutions <- databraryr::list_institutions(vb = vb)
  
  if (!is.null(institutions)) {
    now_str <- lubridate::as_datetime(lubridate::now()) |>
      stringr::str_replace_all(pattern = "[: \\.]", "-")
    
    fn <- file.path(file_path, paste0("institutions-", 
                                      now_str, ".csv"))
    
    readr::write_csv(institutions, file = fn)
    if (vb) message(fn, " saved.")
  } else {
    if (vb) message("Unable to generate institution list.")
  }
}