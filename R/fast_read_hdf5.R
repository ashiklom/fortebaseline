fast_read_hdf5 <- function(files, vars = cohort_vars(), chunk = 1000) {
  files_split <- split(files, ceiling(seq_along(files) / chunk))
  var_string <- paste("-d", vars)
  ## pb <- progress::progress_bar$new(total = length(files_split))
  out <- furrr::future_map(
    files_split,
    function(f) {
      ## pb$tick()
      system2("h5dump", c(var_string, f), stdout = TRUE) 
    },
    .progress = TRUE
  )
}
