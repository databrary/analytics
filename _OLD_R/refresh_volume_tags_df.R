refresh_volume_tags_df <- function(vol_ids = 1:100, vb = FALSE, rq = NULL) {
  message(
    "Refreshing tags & keywords data for volumes ",
    min(vol_ids),
    ":",
    max(vol_ids),
    ". Please be patient."
  )
  purrr::map_df(
    .x = vol_ids,
    .f = make_volume_tags_df,
    vb = vb,
    rq = rq,
    .progress = "Vol tags:"
  )
}

#-------------------------------------------------------------------------------
make_volume_tags_df <- function(vol_id, vb = FALSE, rq = NULL) {
  if (vb)
    message(paste0("Gathering tags from volume ", vol_id))
  these_tags <-
    databraryr::list_volume_tags(vol_id, vb = vb, rq = rq)
  
  # Define NULL return value
  df <- data.frame(
    vol_id = vol_id,
    url = paste0("https://nyu.databrary.org/volume/",
                 vol_id),
    tags = NA,
    weight = NA
  )
  
  if (!is.null(these_tags)) {
    if ("tag_id" %in% names(these_tags)){
      these_tags <- these_tags %>%
        dplyr::select(., tag_id, tag_weight) %>%
        dplyr::rename(., tags = tag_id,
                      weight = tag_weight)
      df <- these_tags
      df$vol_id = vol_id
      df$url <- paste0("https://nyu.databrary.org/volume/", vol_id)      
    }
  }
  dplyr::select(df, vol_id, url, tags, weight)
}

#-------------------------------------------------------------------------------
update_vol_tags_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "databrary-tags.csv") {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }
