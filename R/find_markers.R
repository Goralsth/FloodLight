#' @title Identify Marker Genes
#'
#' @description identifies marker genes from atlas data
#'
#' @param df dataframe with your counts data
#' @param metadata Dataset of metadata
#' @param min_spot_size Minimum size of spot desired
#' @param max_spot_size Maximum size of spot desired
#'
#'
#' @export
#'
#'
find_markers <- function(df) {
  A <- t(df)

  # filter genes in top 50%ile
  mean_expr <- colMeans(A)
  keep_genes <- which(mean_expr > median(mean_expr))
  A <- A[, keep_genes]

  # cluster annotations as a factor vector
  clusters <- downsampled_metadata$subclass_label
  clusters <- as.factor(clusters)
  # call marker genes

  # call marker genes
  markers <- do.call(cbind, lapply(levels(clusters), function(cluster) {
    in_cluster <- colMeans(A[which(clusters == cluster), ])
    out_cluster <- colMeans(A[-which(clusters == cluster), ])
    (in_cluster - out_cluster) / (in_cluster + out_cluster)
  }))
}
