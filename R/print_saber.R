#' @title Print function for SABeR objects
#'
#' @param x [\code{saber}] \cr
#'   SABeR object

#' 
#' @export

print.saber = function(x) {
  print(x[c("test.complete.data", "test.clusters", "clusters")])
}