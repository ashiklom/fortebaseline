#' UMBS soil date for inputs to ED2
#'
#' @return `data.frame` containing soil inputs
#' @author Alexey Shiklomanov
#' @export
umbs_soil <- function() {
  system.file("soil-moisture.csv", package = "fortebaseline") %>%
    read.csv(header = TRUE, stringsAsFactors = FALSE) %>%
    # Make depth negative
    dplyr::mutate(depth = -depth) %>%
    # Start with deepest layer
    dplyr::arrange(depth)
}
