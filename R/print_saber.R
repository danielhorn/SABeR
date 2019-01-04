#' @title Print function for SABeR objects
#'
#' @param x [\code{saber}] \cr
#'   SABeR object
#' @param ... \cr
#'   Not used

#' 
#' @export

print.saber = function(x, ...) {
  print(x[c("test.complete.data", "test.clusters", "clusters", "cluster.perf")])
}