create_new_applicant_list <- function(party_ids = 1:10) {
  
  purrr::map(party_ids, databraryr::download_party, .progress = TRUE) |>
    purrr::list_rbind()
} 