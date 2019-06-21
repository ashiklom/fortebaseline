#' Sort runs into clusters based on most dissimilar LAI
#'
#' @param dat Input `data.frame`
#' @param ... Ignored (necessary for this to work with
#'   [dplyr::group_modify()])
#' @return `data.frame` containing `run_id` and `cluster` 
#' @author Alexey Shiklomanov
#' @export
create_run_groups <- function(dat, ..., n = 4) {
  wide <- dat %>%
    dplyr::select(year, pft, run_id, lai) %>%
    tidyr::spread(run_id, lai, fill = 0) %>%
    dplyr::select(-year, -pft)
  mat <- t(as.matrix(wide))
  km <- kmeans(mat, n)
  tibble::enframe(km$cluster, "run_id", "cluster") %>%
    dplyr::mutate(run_id = as.numeric(run_id))
}
