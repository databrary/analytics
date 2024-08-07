#' Update The Largest Party and Volume Identifiers
#' 
#' @param csv_dir A character string. Default is "src/csv".
#' @param default_vol An integer. Default is 1567.
#' @param default_party An integer. Default is 10941.
#' @vb A logical value. Show verbose output.
#'
update_max_vol_party_ids <-
  function(csv_dir = "src/csv",
           default_vol = 1567,
           default_party = 10941,
           default_increment = 100,
           update_vol = TRUE,
           update_party = TRUE,
           vb = FALSE) {
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    fn <- file.path(csv_dir, 'max-ids.csv')
    if (!file.exists(fn)) {
      if (vb)
        message(paste0('File does not exist: ', fn, "'. Creating."))
      max_ids <-
        data.frame(MAX_VOL_ID = default_vol, MAX_PARTY_ID = default_party)
      readr::write_csv(max_ids, fn)
    }
    old_max_ids <- readr::read_csv(fn, show_col_types = FALSE)
    if (is.data.frame(old_max_ids)) {
      if (vb)
        message("Updating max vol and party IDs.")
      new_max_vol_id <- old_max_ids$MAX_VOL_ID
      if (update_vol) {
        new_max_vol_id <- get_max_vol_id(old_max_ids$MAX_VOL_ID, 
                                         increment = default_increment, vb = vb)        
      }
      
      if (update_party) {
        new_max_party_id <- get_max_party_id(old_max_ids$MAX_PARTY_ID, 
                                             increment = default_increment, vb = vb)        
      }
      
      max_ids <-
        data.frame(MAX_VOL_ID = new_max_vol_id, MAX_PARTY_ID = new_max_party_id)
      if (vb)
        message("Writing IDs to '", fn, "'.")
      readr::write_csv(max_ids, fn)
    } else {
      if (vb)
        message("Error opening '", fn, '.')
      return(NULL)
    }
    max_ids
  }

#-------------------------------------------------------------------------------
get_max_vol_id <- function(start_id = 1568,
                           increment = 20,
                           vb = FALSE) {
  stopifnot(is.numeric(start_id))
  stopifnot(start_id > 0)
  stopifnot(is.numeric(increment))
  stopifnot(increment > 0)
  
  if (vb) message("Determining max vol_id.")
  v_ids <- start_id:(start_id + increment)
  vs_exist <- purrr::map(v_ids, vol_id_exists, vb = vb, .progress = TRUE) |>
    unlist()
  
  new_max <- max(v_ids[vs_exist])
  new_max
}

#-------------------------------------------------------------------------------
get_max_party_id <- function(start_id = 10922,
                             increment = 20,
                             vb = FALSE) {
  stopifnot(is.numeric(start_id))
  stopifnot(start_id > 0)
  stopifnot(is.numeric(increment))
  stopifnot(increment > 0)
  
  if (vb) message("Determining max party_id.")
  p_ids <- start_id:(start_id + increment)
  ps_exist <- purrr::map(p_ids, party_id_exists, vb = vb, .progress = TRUE) |>
    unlist()
  
  new_max <- max(p_ids[ps_exist])
  new_max
}

#-------------------------------------------------------------------------------
vol_id_exists <- function(vol_id = 1, vb = FALSE) {
  if (vb) message("Accessing vol_id ", vol_id, ".")
  v = databraryr::get_volume_by_id(vol_id, vb = vb)
  
  if (is.null(v)) {
    FALSE
  } else {
    TRUE
  }
}

#-------------------------------------------------------------------------------
party_id_exists <- function(party_id = 1, vb = FALSE) {
  if (vb) message("Accessing party_id ", party_id)
  p = databraryr::get_party_by_id(party_id, vb = vb)
  
  if (is.null(p)) {
    FALSE
  } else {
    TRUE
  }
}
