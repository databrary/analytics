likely_fake <- function(party_id, affiliation_txt = "george") {
  p <- databraryr::get_party_by_id(party_id)
  result <- NULL
  if ("affiliation" %in% names(p)) {
      if (stringr::str_detect(p$affiliation, affiliation_txt)) {
        result <- tibble::tibble(id = p$id, sortname = p$sortname, prename = p$prename,
                       affiliation = p$affiliation, parents = p$parents, children = p$children,
                       access = p$access)
      }
  }
  result
}

make_likely_fake_df <- function(start_id, end_id, affiliation_txt = "george") {
  purrr::map(start_id:end_id, likely_fake, .progress = TRUE) |>
    purrr::list_rbind()
  
}