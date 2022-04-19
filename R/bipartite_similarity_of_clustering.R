#' @title Calculate similarity of two clustering vectors
#'
#' @description This function aligns two clustering vectors on a bipartite adjacency matrix using Jaccard overlap,
#' and returns the cost of this alignment as a measure of the parsimony of alignment
#'
#' @param vec1 a clustering vector with \code{n} unique idents for \code{m} samples
#' @param vec2 a clustering vector with \code{n} unique idents for \code{m} samples, directly corresponds with \code{vec1}
#' @returns cost of bipartite matching on a Jaccard distance cost matrix between the two clusterings
#'
bipartite_similarity_of_clustering <- function(vec1, vec2){

  # get unique cluster identifiers and initialize similarity matrix
  idents1 <- unique(vec1)
  idents2 <- sort(unique(vec2))

  if(length(idents1) != length(idents2))
    stop("both cluster vectors should contain an equal number of cluster labels")

  jaccard_similarity <- matrix(0, length(idents1), length(idents2))
  colnames(jaccard_similarity) <- idents2
  rownames(jaccard_similarity) <- idents1

  # populate similarity matrix to calculate Jaccard distance between each cluster pair between both vectors
  for(i1 in 1:length(idents1)){
    for(i2 in 1:length(idents2)){
      intersection <- sum(vec1 == idents1[[i1]] & vec2 == idents2[[i2]])
      union <- sum(vec1 == idents1[[i1]] | vec2 == idents2[[i2]])
      jaccard_similarity[i1, i2] <- intersection / union
    }
  }

  # calculate cost of bipartite matching on a Jaccard distance cost matrix
  matching <- RcppML::bipartiteMatch(1 - jaccard_similarity)
  matching <- list("similarity" = 1 - matching$cost / length(idents1), "pairs" = matching$pairs)
  return(matching)
}
