#' @title Statistical Analysis of Benchmark Results
#'
#' @description
#' TODOTODOTODOTODOTODOTODO
#'
#' @param data [\code{saber}] \cr
#'   Result of saberIt
#' @return Bisschen Code of der Konsole
#' 
#' @export

summarizeCluster = function(saber.result) {
  
  data <- saber.result$pars$data
  expParName <- saber.result$pars$expParName
  
  data <- data[!duplicated(data$.expID), ]
  n <- nrow(data)
  
  clusters <- split(data[, expParName], data$.clustID)
  
  lapply(seq_along(clusters), function(i) singlePrint(clusters[[i]], i, n))
  
  plot.data <- data[, expParName]
  
  plot.data <- plot.data[, sapply(plot.data, is.numeric)]
  
  for (col in seq_along(plot.data)) {
    plot.data[, col] = jitter(plot.data[, col], 0.5)
  }
  
  plot(plot.data, col = data$.clustID)
  
  return(invisible(clusters))
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
