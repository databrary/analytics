get_all_owners_save_csvs <-
  function(max_vol_id = 1520,
           dir = "src/csv",
           vb = FALSE) {
    stopifnot(is.numeric(max_vol_id))
    stopifnot(max_vol_id > 0)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    # TODO: Fix this hack
    get_save_volumes_owners(1, 500)
    get_save_volumes_owners(501, 1000)
    get_save_volumes_owners(1001, 1275) # skip 1276 & 1277 because no owners
    get_save_volumes_owners(1278, 1500)
    get_save_volumes_owners(1501, max_vol_id)
  }
