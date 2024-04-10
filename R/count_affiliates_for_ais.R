#-------------------------------------------------------------------------------
count_affiliates_for_ais <- function(ai_ids) {
  purrr::map_dbl(ai_ids, count_affiliates_for_ai)
}

count_affiliates_for_ai <- function(ai_id) {
  affils <- databraryr::list_affiliates(ai_id)
  if (is.null(affils)) {
    x <- 0
  } else {
    x <- dim(affils)[1]
  }
  x
}