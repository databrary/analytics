make_stem_tags_df <- function(tags_df,
                              vb = FALSE,
                              save_csv = TRUE) {
  stem_tags <- dplyr::filter(tags_df, tags %in% select_tags)
  stem_tags <- dplyr::arrange(stem_tags, vol_id, tags)
  
  # Unique vol ids to get volume metadata, esp title
  stem_vol_ids <- unique(stem_tags$vol_id)
  
  # Pull titles
  if (vb)
    message("Gathering STEM-related tags from n=",
            length(stem_vol_ids),
            " volumes.")
  stem_vols_df <- purrr::map_df(
    .x = stem_vol_ids,
    .f = databraryr::list_volume_metadata,
    .progress = "STEM tags:"
  )
  
  stem_vols_df <-
    dplyr::left_join(stem_vols_df, stem_tags, multiple = "all")
  
  stem_vols_df <- stem_vols_df %>%
    dplyr::filter(., vol_id != 109) %>% # Empty volume
    dplyr::select(., -owners, -permission, -doi)
  
  stem_vols_df
}