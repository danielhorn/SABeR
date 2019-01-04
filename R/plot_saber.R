#' @title Plot function for \code{saber} objects
#'
#' Visualizes the results of a saber analysis with directed graphs.
#'
#' @param x [\\code{saber.result}] \cr
#'   Result from \code{saberIt}.
#' @param test.niveaus [\code{numeric()}] \cr
#'   Which test niveaus should be visualized? At most, 4 niveaus can be specified.
#' @param lty [\code{character()}]\cr
#'   Line types for the test niveaus. Must have the same length as \code{test.niveaus},
#'   allowed values are the linetypes of \code{plot.igraph}.
#' @param radius [\code{numeric(1)}]
#'   Radius of nodes. Default is 48, should be increased for long algorithm names.
#' @param plot.legend [\code{logical(1)}]
#'   Should a legend be included? Default is \code{TRUE}.
#' @param ... [any] \cr
#'   Additional parameters for \code{plot.igraph}.
#' @return \code{invisible(NULL)}, as a side effect, a plot is generated.
#'
#'
#' @export

plot.saber = function(x,
  test.niveaus = c(1e-10, 1e-5, 1e-2, 5e-2),
  lty = c("solid", "longdash", "dotdash", "dotted")[1:length(test.niveaus)],
  radius = 48, print.legend = TRUE, ...) {
  
  assertNumeric(test.niveaus, lower = 0, upper = 1, min.len = 1, max.len = 4)
  assertSubset(lty, c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))
  assertNumber(radius, lower = 0)
  assertFlag(print.legend)
  
  
  od = order(test.niveaus, decreasing = TRUE)
  test.niveaus = test.niveaus[od]
  lty = lty[od]
  
  saber.result = x
  algo.names = levels(saber.result$pars$data[, saber.result$pars$algoName])
  
  # First: Plot for test on complete data set
  plot.data = getEdges(
    saber.result$test.complete.data$ranks,
    saber.result$test.complete.data$post.hoc,
    algo.names, test.niveaus)
  
  makeSinglePlot(
    vertices = algo.names,
    vertex.labels = paste(algo.names,"\n", round(saber.result$test.complete.data$ranks, digits = 4)),
    order = order(saber.result$test.complete.data$ranks),
    edges = plot.data$edges,
    line.types = lty[plot.data$line.types],
    radius = radius,
    subtitle = sprintf("Friedmann test p-Value:\n %4g", saber.result$test.complete.data$global),
    main = "Global test results", edge.arrow.size = 1,
    ...)
  
  
  # Set plot grid
  n.plots = length(saber.result$test.clusters$global) + print.legend
  h = ceiling(sqrt(n.plots))
  v = if(h * (h - 1) >= n.plots) h - 1 else h
  par(mfrow = c(v, h))
  on.exit(par(mfrow = c(1, 1)))
  
  # Second plots: Test result in clusters
  for (i in seq_along(saber.result$test.clusters$global)) {
    plot.data = getEdges(
      saber.result$test.clusters$ranks[i, ],
      saber.result$test.clusters$post.hoc[[i]],
      algo.names, test.niveaus)
    
    makeSinglePlot(
      vertices = algo.names,
      vertex.labels = paste(algo.names,"\n", round(saber.result$test.clusters$ranks[i, ], digits = 4)),
      order = order(saber.result$test.clusters$ranks[i, ]),
      edges = plot.data$edges,
      line.types = lty[plot.data$line.types],
      radius = radius,
      subtitle = sprintf("Friedmann test p-Value:\n %4g", saber.result$test.clusters$global[i]),
      main = paste0("Results in cluster ", i),
      ...)
  }
  
  # Add legend for test niveau linetype
  if (print.legend) {
    plot.new()
    legend(title = "Test Niveaus",
      "center",
      legend = test.niveaus, 
      lty = lty,
      col = "black",
      bty = "n",
      cex = 1)
  }
  
  return(invisible(NULL))
}
