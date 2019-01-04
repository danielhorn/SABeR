#' @title summarizeCluster
#'
#' @description
#' Summary of the clusters found in the saber analysis
#'
#' @param data [\code{saber}] \cr
#'   Result of saberIt
#' @return Summary printed on the console as a side effect, a ggplot object is returned
#' 
#' @export

summarizeCluster = function(saber.result) {
  
  data = saber.result$pars$data
  expParName = saber.result$pars$expParName
  
  data = data[!duplicated(data$.expID), ]
  n = nrow(data)
  
  clusters = split(data[, expParName], data$.clustID)
  
  lapply(seq_along(clusters), function(i) singlePrint(clusters[[i]], i, n))
  
  # Scatterplot matrix
  plot.data = data[, expParName]
  
  plot.data = plot.data[, sapply(plot.data, is.numeric)]
  
  for (column in seq_along(plot.data)) {
    plot.data[, column] = jitter(plot.data[, column], 0.5)
  }
  
  plot.data$.clustID = as.factor(data$.clustID)
  p = GGally::ggpairs(plot.data, columns = 1:3, ggplot2::aes(colour = .clustID))
  
  return(p)
}


singlePrint = function(exp.data, i, n) {
  numeric = sapply(exp.data, is.numeric)
  
  catf("\n")
  catf("Cluster %i contains %i out of %i experiments.", i, nrow(exp.data), n)
  #catf("Mean values of discrete parameters are:")
  #print(colMeans(exp.data[, numeric]))
  catf("\n")
  catf("Summary of the data set:")
  print(summary(exp.data))
  catf("===========================================\n")
  
  
  return(invisible(NULL))
}
