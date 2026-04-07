export_institution_affiliates <- function(institution_id = 8, 
                                          file_path = "private/inst_affils",
                                          vb = FALSE) {
  assertthat::assert_that(length(institution_id) == 1)
  assertthat::is.number(institution_id)
  assertthat::assert_that(institution_id > 0)

  assertthat::assert_that(length(file_path) == 1)
  assertthat::is.string(file_path)
  assertthat::is.writeable(file_path)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  affils <- databraryr::list_institution_affiliates(institution_id, vb = vb)
  if (!is.null(affils)) {
    fn <- file.path(file_path, paste0("inst-", 
                                      stringr::str_pad(institution_id, width = 5, pad = "0"),
                                      "-affils.csv"))

    readr::write_csv(affils, file = fn)
    if (vb) message(fn, " saved.")
  } else {
    if (vb) message("No affiliates for institution ", institution_id)
  }
}