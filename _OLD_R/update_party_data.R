update_party_data <- function(party_id = 6,
                              csv_dir = "src/csv",
                              save_csvs = TRUE,
                              vb = FALSE,
                              rq = NULL) {
  assertthat::is.number(party_id)
  assertthat::assert_that(party_id > 0)
  assertthat::assert_that(length(party_id) == 1)
  
  assertthat::is.string(csv_dir)
  assertthat::assert_that(length(csv_dir) == 1)
  assertthat::is.writeable(csv_dir)
  
  assertthat::assert_that(is.logical(save_csvs))
  assertthat::assert_that(length(save_csvs) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.list(rq) | is.null(rq))
  
  # Get party blob
  if (databraryr::is_institution(party_id)) {
    if (vb)
      message("Party ", party_id, " is not an individual")
    return(NULL)
  } else {
    if (vb)
      message("Retrieving data for party ", party_id)
    party_data <-
      databraryr::get_party_by_id(party_id, vb = vb, rq = rq)
    if (is.null(party_data)) {
      if (vb)
        message("No data for party ", party_data)
      return(NULL)
    }
  }
  
  # party data
  if (vb)
    message("Saving info for party ", party_id)
  party_fn <-
    paste0("party-",
           stringr::str_pad(party_id, 5, pad = "0"),
           "-info.csv")
  party_full_fn <- file.path(csv_dir, party_fn)
  tibble::tibble(
    party_id = party_id,
    party_last = party_data$sortname,
    party_first = party_data$prename,
    party_orcid = party_data$orcid,
    party_affiliation = party_data$affiliation,
    party_url = party_data$url
  ) |>
    readr::write_csv(file = party_full_fn)
  
  # party affiliates/children
  if (is.null(party_data$children)) {
    if (vb)
      message("No affiliates for party ", party_id)
  } else {
    party_affils <- purrr::map(party_data$children, as.data.frame) |>
      purrr::list_rbind()
    if (dim(party_affils)[1] == 0) {
      if (vb)
        message("Party ", party_id, " has no affiliates")
    } else {
      if (vb)
        message("Saving affiliate info for party ", party_id)
      party_affils_fn <-
        paste0("party-",
               stringr::str_pad(party_id, 5, pad = "0"),
               "-affiliates.csv")
      party_affils_full_fn <- file.path(csv_dir, party_affils_fn)
      party_affils |>
        dplyr::select(all_of(
          c(
            "party.id",
            "party.sortname",
            "party.prename",
            "party.affiliation"
          )
        )) |>
        dplyr::rename(
          party_id = party.id,
          party_last = party.sortname,
          party_first = party.prename,
          party_affiliation = party.affiliation
        ) |>
        readr::write_csv(file = party_affils_full_fn)
    }
  }
  
  # party sponsors/parents
  if (vb) message("Party sponsors...")
  if (is.null(party_data$parents)) {
    if (vb)
      message("No sponsors for party ", party_id)
  } else {
    party_sponsors <- purrr::map(party_data$parents, as.data.frame) |>
      purrr::list_rbind()
    if (dim(party_sponsors)[1] == 0) {
      if (vb)
        message("Party ", party_id, " has no sponsors")
    } else {
      if (vb)
        message("Saving sponsor info for party ", party_id)
      party_sponsors_fn <-
        paste0("party-",
               stringr::str_pad(party_id, 5, pad = "0"),
               "-sponsors.csv")
      party_sponsors_full_fn <-
        file.path(csv_dir, party_sponsors_fn)
      is_institution <-
        ifelse(
          is.na(party_sponsors$party.institution),
          FALSE,
          party_sponsors$party.institution
        )
      party_sponsors |>
        dplyr::select(all_of(
          c(
            "party.id",
            "party.sortname"
          )
        )) |>
        dplyr::rename(
          party_id = party.id,
          party_last = party.sortname
        ) |>
        dplyr::mutate(party_is_institution = is_institution) |>
        readr::write_csv(file = party_sponsors_full_fn)
    }
  }
  
  # volume access
  if (vb) message("Party access to volumes...")
  if (is.null(party_data$access)) {
    if (vb)
      message("Party ", party_id, " has no access to volumes")
  } else {
    party_access <- purrr::map(1:100,
                               extract_party_volume_info, 
                               party_data$access, vb = vb) |>
      purrr::list_rbind() |>
      dplyr::mutate(party_id = party_id)
    if (dim(party_access)[1] == 0) {
      if (vb)
        message("Party ", party_id, " has no access to volumes")
    } else {
      if (vb)
        message("Saving volume access info for party ", party_id)
      party_access_fn <-
        paste0("party-",
               stringr::str_pad(party_id, 5, pad = "0"),
               "-access.csv")
      party_access_full_fn <-
        file.path(csv_dir, party_access_fn)
      party_access |>
        readr::write_csv(file = party_access_full_fn)
    }
  }
}

#-------------------------------------------------------------------------------
extract_party_volume_info <-
  function(i, p_access_list, vb = FALSE) {
    if (vb)
      message("index: ", i)
    x <- purrr::pluck(p_access_list, i, 3) |>
      tibble::as_tibble()
    if ("id" %in% names(x)) {
      x |>
        dplyr::select(id, name, creation, permission, owners, publicsharefull) |>
        dplyr::rename(
          vol_id = id,
          vol_name = name,
          vol_creation = creation,
          vol_owners = owners,
          vol_permission = permission,
          vol_public = publicsharefull
        )
    } else {
      return(NULL)
    }
  }
