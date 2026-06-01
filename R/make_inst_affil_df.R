make_inst_affil_df <- function() {
  inst_invest_df <- load_institutions_invest_data()
  inst_df <- load_inst_df()
  dplyr::left_join(inst_df, inst_invest_df, by = "id")
}