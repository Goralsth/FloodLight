
#' @title Average Expression of cell types
#'
#' @description used the generate the average expression profile of each cell type
#'
#' @param data A matrix of counts data in structure  cells x genes
#' @param group vector containing cell type for each cell in counts data
#'
#' @export


average_expression<-function(data, group){
  result <- do.call(cbind, lapply(unique(group,na.rm=TRUE), function(x) rowMeans(data[, which(group == x)])))
  colnames(result) <- unique(group, na.rm=TRUE)
  result
}
