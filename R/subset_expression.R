#' @title Subset the counts data by the Markers Threshold, then ensure expression levels are equal for all columns
#'
#' @description subsetting data
#'
#' @param markers_threshold Your markers_threshold dataset generated from Identify Marker Thresholding function
#' @param counts Your- raw counts matrix
#' @param Value Your threshold for how close you want your total number of transcripts to be to the minimum. E.G. all columns within 150% of the minimum, enter 1.5.
#'
#' @export
#'
#'
subset_expression <- function(markers_threshold, cluster_counts, Value) {

  count1 <-cluster_counts
    names1 <- rownames(markers_threshold)
  count1 <- count1[rownames(count1) %in% names1, ]
  sum1 <- Matrix::colSums(count1)
  min1 <- min(sum1)
  a<-(Value*min1)
  if (all((a > sum1))) {
    return(count1)
  }

  else {



    while (any((a < sum1))) {

      values <- as.numeric(which.min(sum1))

      markers_threshold2 <- markers_threshold[-values,]
      markers_threshold2 <- as.data.frame(lapply(markers_threshold2, function(x) x[-which.min(x)])) # I think this will work?...maybe?
      DF1 <- rbind(markers_threshold2, markers_threshold[,!names(markers_threshold) %in% names(markers_threshold2)])

      # I think this works but i dont know

      name1 <- rownames(DF1)
      count1 <- count1[rownames(count1) %in% name1, ]
      sum1 <- colSums(count1)
      min1 <- min(sum1)
      a<-(Value*min1)
    }
return(a)

  }
}

