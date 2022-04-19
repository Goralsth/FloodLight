---
title: "Generate_Spots"
author: "Zach Dubrine,Thomas Goralski"
date: '2022-04-01'
output: html_document
---








#' @title Generate random spots of given max and min quantity size
#'
#' @description generate random spots
#'
#' @param df A dataframe you are generating spot from
#' @param metadata Dataset of metadata
#' @param min_spot_size Minimum size of spot desired
#' @param max_spot_size Maximum size of spot desired
#'
#' @export
#'
generate_spots<-function(A, cluster_ids, min_spot_size = 2, max_spot_size = 8){

  # filter out NAs in case there are NAs in cluster idents
  cluster_ids <- as.factor(cluster_ids)
  stopifnot(length(cluster_ids) == ncol(A))
  if(sum(is.na(cluster_ids)) > 0) {
    ind <- which(is.na(cluster_ids))
    cluster_ids <- cluster_ids[-ind]
    A <- A[, -ind]
    warning(length(ind), " 'NA' values were found in 'cluster_ids'. Removing these values and corresponding cells from 'A'")
  }




  # construct sparse matrix of cell identity (rows are cluster ids, columns are cells)
  spot_counts <- new("ngCMatrix", Dim = c(length(levels(cluster_ids)), ncol(A)), p = 0:ncol(A), i = as.integer(as.numeric(cluster_ids) - 1))
  rownames(spot_counts) <- levels(cluster_ids)
  colnames(spot_counts) <- colnames(A)
  v <- 1:ncol(A)

  # construct list of spots
  spots <- list()
  while(length(v) > max_spot_size){
    indices <- sample(1:length(v), sample(min_spot_size:max_spot_size, 1))
    spots[[length(spots) + 1]] <- v[indices]
    v <- v[-indices]
  }
  spots[[length(spots) + 1]] <- v

  # generate spot matrix
  A_ <- do.call(cbind, lapply(spots, function(spot) rowSums(A[, spot])))
  meta <- do.call(cbind, lapply(spots, function(spot) rowSums(spot_counts[, spot])))
  colnames(A_) <- colnames(meta) <- paste0("spot", 1:ncol(A_))
  list("spots" = A_, "cell_types" = meta)
}





