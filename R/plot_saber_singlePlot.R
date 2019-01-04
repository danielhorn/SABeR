# Work horse for the plot itself: Gets all information needed for the plot
# and does the "nice work"

# gets:
#  vertices: names of vertices to be plotted
#  vertex.labels: labels for the vertices
#  order: order of the vertices
#  radius: size of vertices
#  arrows: Vector of lines to be plotted
#  line.types: numeric vector, which lines should be plotted with the same type
#  subtitle: subtitle for the graph
#  main: character, title of plot

# additional args to plot.igraph allowed

makeSinglePlot = function(vertices, vertex.labels, order, radius, edges, line.types,  subtitle, main, ...) {
  
  args = list(...)
  edge.arrow.size = coalesce(args$edge.arrow.size, 0.1)
  vertex.label.cex = coalesce(args$vertex.label.cex, 1)
  vertex.label.color = coalesce(args$vertex.label.color, "black")
  edge.width = coalesce(args$edge.width, 1)
  
  # Create igraph objject with algorithms ordered in a circle
  graphobject = make_empty_graph() + vertices(vertices) + edges(edges)
  coords = layout_in_circle(graphobject, order = order)
  
  # Parameters of the edges:
  k = gsize(graphobject)
  edge_attr(graphobject) = list(color = rep("black", k) )
  
  ############################################################################
  # Parameters of the vertexes
  vertex_attr(graphobject) = list(
    name = vertex.labels,
    color = rep("white", length(vertices)),
    size = rep(radius, length(vertices)),
    shape = rep("circle", length(vertices))
  )
  
  vertex_attr(graphobject)$color[order[1]] = rgb(255, 83, 40, maxColorValue = 256)
  
  # Do the Plot
  plot(graphobject, 
    rescale = FALSE,
    layout = coords,
    edge.lty = line.types,
    edge.arrow.size = edge.arrow.size,
    vertex.label.cex = vertex.label.cex,
    vertex.label.color = "black",
    edge.width = edge.width,
    main = main,sub = subtitle,
    ...)
  
  #title(sub = subtitle, cex.sub = 1, main = main)
}