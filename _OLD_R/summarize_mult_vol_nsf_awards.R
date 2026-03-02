#' Summarize NSF Award Data For Multiple Databrary Volumes.
#'
#' @param vol_ids Numeric array of one or more Databrary volume IDs.
#' @returns A data frame (tibble) that combines the Databrary and NSF database
#' information about the NSF awards reported in `vol_ids`.
#' @example summarize_mult_vol_nsf_awards() # Summarizes NSF funding for Databrary
#' volumes 1 through 25.
#' @export
summarize_mult_vol_nsf_awards <- function(vol_ids = 1:25, vb = FALSE) {
  stopifnot(is.numeric(vol_ids))
  stopifnot(sum(vol_ids > 0) == length(vol_ids))
  stopifnot(min(vol_ids) < max(vol_ids))
  
  message("Retrieving NSF award info for Databrary volumes ", min(vol_ids), ":", max(vol_ids))
  purrr::map(vol_ids, merge_databrary_nsf_award_df, vb = vb,
             .progress = "NSF awards for vols: ") |>
    purrr::list_rbind()
}

#------------------------------------------------------------------------------
add_clean_nsf_award_id <-
  function(nsf_award_df = get_vol_nsf_awards(),
           vb = FALSE) {
    stopifnot(is.data.frame(nsf_award_df))
    
    if (is.null(nsf_award_df))
      return(NULL)
    
    cleaned_ids <-
      purrr::map(nsf_award_df$funder_award, extract_award_id, .progress = vb) |> 
      unlist()
    nsf_award_df$award_id <- cleaned_ids
    nsf_award_df
  }

#------------------------------------------------------------------------------
merge_databrary_nsf_award_df <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.logical(vb))
  
  # Get NSF award(s) info from Databrary
  if (vb) message("Volume: ", vol_id)
  db_nsf_awards_df <- get_vol_awards(vol_id, funder_str = "NSF")
  
  # Add cleaned NSF award IDs, lookup data on NSF, and combine Databrary with
  # NSF info
  if (!is.null(db_nsf_awards_df)) {
    if (vb) message(" Cleaning award IDs")
    db_nsf_awards_clean_df <- db_nsf_awards_df |>
      add_clean_nsf_award_id()
    
    if (vb) message(" Retrieving award info from NSF")
    nsf_awards_df <-
      get_mult_nsf_awards(db_nsf_awards_clean_df$award_id)
    
    if (dim(nsf_awards_df)[1] < 1) {
      message(" No NSF data found for award ", db_nsf_awards_clean_df$award_id)
      return(NULL)
    }
    
    if (vb) message(" Merging Databrary & NSF data")
    dplyr::left_join(db_nsf_awards_clean_df,
                     nsf_awards_df,
                     by = c("award_id" = "id")) |>
      dplyr::select(-funder_id, -award, -agency) |>
      # Some NSF database cities are in all caps
      dplyr::mutate(awardeeCity = stringr::str_to_title(awardeeCity))
    # TODO: Consider rectifying camel case with snake case field names
  } else {
    if (vb) message(" No NSF awards in volume ", vol_id)
    NULL
  }
}

#------------------------------------------------------------------------------
get_mult_nsf_awards <-
  function(ids = c("1238599", "1147440"),
           vb = FALSE) {
    purrr::map(ids, get_nsf_data_for_award_id, .progress = vb) |>
      purrr::list_rbind()
  }

#------------------------------------------------------------------------------
extract_award_id <- function(award_str = "BCS-1238599") {
  stopifnot(is.character(award_str))
  
  award_id <- stringr::str_remove_all(award_str, "-") |>
    stringr::str_extract("[0-9]+")
  award_id
}

#------------------------------------------------------------------------------
get_vol_awards <- function(vol_id = 1,
                           funder_str = ".*",
                           vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(length(vol_id) > 0)
  stopifnot(is.character(funder_str))
  stopifnot(is.logical(vb))
  
  vol_awards <- databraryr::list_volume_funding(vol_id)
  if (dim(vol_awards)[1] == 0) {
    if (vb) message("No awards in volume_id ", vol_id, ".")
    return(NULL)
  } else {
    these_awards <- dplyr::filter(vol_awards,
                                  stringr::str_detect(funder_name, funder_str))
    if (dim(these_awards)[1] == 0) {
      return(NULL)
    } else {
      these_awards
    }
    
  }
}
