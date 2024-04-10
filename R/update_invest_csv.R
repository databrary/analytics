update_invest_csv <- function(all_inst_df,
                              csv_dir = "src/csv",
                              vb = FALSE) {
  stopifnot(is.data.frame(all_inst_df))
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  stopifnot(is.logical(vb))
  
  if (vb)
    message("Filtering for active institutions with AIs.")
  inst_ids <-
    dplyr::filter(all_inst_df, daa == TRUE, n_auth_invest > 0) |>
    dplyr::select(inst_id)
  ids <- as.integer(unlist(inst_ids))
  
  if (vb)
    message("There are n=",
            dim(ids)[1],
            " institutions with AIs. Retrieving AI info.")
  
  ais_l <-
    purrr::map(ids, get_ais_from_inst, vb = vb, rq = rq,
               .progress = "AIs from insts:")
  
  if (vb)
    message("Making data frame.")
  ais_df <- purrr::list_rbind(ais_l)
  
  fn <- file.path(csv_dir, "all-ais.csv")
  if (vb)
    message("Writing CSV: ", fn)
  readr::write_csv(ais_df, fn)
  
  fn
}

#-------------------------------------------------------------------------------
get_ais_from_inst <- function(inst_id = 8, vb = NULL, rq = NULL) {
  if (vb)
    message("Getting AIs from institution ", inst_id)
  #inst_df <- databraryr::list_party(inst_id)
  inst_df <- databraryr::get_party_by_id(inst_id, vb = vb, rq = rq)
  
  if (!is.null(dim(inst_df$children))) {
    #ais_df <- as.data.frame(inst_df$children$party)
    ais_df <- purrr::map(inst_df$children, as.data.frame) %>%
      purrr::list_rbind()
    
    # ais_df <- dplyr::rename(ais_df,
    #                         ai_id = id,
    #                         ai_last = sortname,
    #                         ai_first = prename)
    
    ais_df <- dplyr::rename(ais_df,
                            ai_id = party.id,
                            ai_last = party.sortname,
                            ai_first = party.prename)
    
    #df <- tibble::tibble(ais_df)
    df <- dplyr::mutate(
      ais_df,
      inst_id = inst_df$id,
      inst_name = inst_df$sortname,
      inst_db_url = paste0("https://nyu.databrary.org/party/", inst_df$id),
      ai_db_url = paste0("https://nyu.databrary.org/party/", ai_id)
    )
    df$n_affils <- count_affiliates_for_ais(df$ai_id)
    
    df <- dplyr::arrange(df, desc(n_affils), ai_last, ai_first)
    df
  } else {
    NULL
  }
}

#-------------------------------------------------------------------------------
count_affiliates_for_ai <- function(ai_id) {
  affils <- databraryr::list_affiliates(ai_id)
  if (is.null(affils)) {
    x <- 0
  } else {
    x <- dim(affils)[1]
  }
  x
}
