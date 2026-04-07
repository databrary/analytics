export_inst_user_sharing_data <- function(inst_id = 46,
                                          input_path = "private/inst_affils",
                                          save_path = "private/inst_invest_shared",
                                          vb = FALSE) {
  assertthat::assert_that(length(inst_id) == 1)
  assertthat::is.number(inst_id)
  assertthat::assert_that(inst_id > 0)
  
  assertthat::assert_that(length(input_path) == 1)
  assertthat::is.string(input_path)
  assertthat::is.writeable(input_path)
  
  assertthat::assert_that(length(save_path) == 1)
  assertthat::is.string(save_path)
  assertthat::is.writeable(save_path)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  # Open institution file under private/inst_affils/inst-nnnnn-affils.csv
  fn_in <- file.path(input_path,
                     paste0(
                       "inst-",
                       stringr::str_pad(inst_id, width = 5, pad = "0"),
                       "-affils.csv"
                     ))
  if (!file.exists(fn_in)) {
    if (vb) message("'", fn_in, "' does not exist. Skipping.\n")
    return(NULL)
  }
  
  if (vb)
    message("Opening '", fn_in, "'.")
  this_inst <- readr::read_csv(fn_in, show_col_types = FALSE)
  
  inst_ai_ids <- this_inst$user_id
  assertthat::is.number(inst_ai_ids)
  if (vb)
    message("Institution ",
            inst_id,
            " has ",
            length(inst_ai_ids),
            " authorized investigator(s). Extracting sharing info for each.\n")
  
  # Extract vols for all users
  inst_ai_sharing_df <- purrr::map(inst_ai_ids, 
                                   list_user_owned_volumes_sharing_info, vb = vb) |>
    purrr::list_rbind()
  
  if (is.null(inst_ai_sharing_df)) return(NULL)
  
  if (dim(inst_ai_sharing_df)[1] > 0) {
    inst_ai_sharing_df <- inst_ai_sharing_df |>
      dplyr::arrange(user_id)
  } else {
    if (vb) message("\nInstitution ",
                    inst_id,
                    " has no users with volumes.")
    return(NULL)
  }
  
  inst_ai_sharing_df$inst_id <- inst_id
  if (vb)
    message("\nInstitution ",
            inst_id,
            " has ",
            dim(inst_ai_sharing_df)[1],
            " users with volumes.")
  
  fn_out <- file.path(save_path,
                      paste0(
                        "inst-",
                        stringr::str_pad(inst_id, width = 5, pad = "0"),
                        "-shared-vols.csv"
                      ))
  # if (vb)
  #   message("Saving '", fn_out, "'.")
  readr::write_csv(inst_ai_sharing_df, fn_out)
  fn_out
}