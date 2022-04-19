#' @title Add column names to Makers Dataset
#'
#' @description adding columns
#'
#' @param metadata original metadata
#' @param clusters MUST BE IN "". Name of cluster
#' @param df marker dataset
#'
#'
#' @export

marker_column_names <- function(metadata, clusters, df) {
  colnames(df) <- unique(metadata$clusters, na.rm = TRUE)
  df
}
