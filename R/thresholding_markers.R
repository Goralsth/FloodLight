#' @title Marker Thresholding function
#'
#' @description threshold for acceptable markers
#'
#' @param markers Your Markers dataset generated from Identify Marker Genes function
#' @param theshold Your threshold for a marker. Must be a postive integer.
#'
#' @export
#'
#'
thresholding_markers <- function(markers, threshold) {
  remove <- function(x) {
    ifelse(x < as.numeric(threshold), 0, x)
  }

  markers_threshold <- apply(markers, c(1, 2), FUN = remove)
  markers_threshold <- as.data.frame(markers_threshold)
  markers_threshold[rowSums(markers_threshold[]) > 0, ]
}
