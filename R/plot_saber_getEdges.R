# Gets the specific test results from the saber procedure,
# i.e. the p.values from the post.hoc tests and the mean ranks for each
# algorithm

# Returns a edges vector for plot.igraph as well linetypes correspondig
# to the specified test niveaus.

getEdges = function(ranks, post.hoc, algo.names, test.niveaus) {
  
  # Connection matrix: which vertices must be connected?
  # All p-values significant to the highest p-value get a connection
  post.hoc = rbind(NA, cbind(post.hoc, NA))
  connections = post.hoc < max(test.niveaus)
  connections[is.na(connections)] = FALSE
  rownames(connections) = colnames(connections) = algo.names
  
  # Now, consider directions of edges. Therefore, first, make it undirected,
  # i.e. copy all edges from the lower to the upper triangle
  connections[upper.tri(connections)] =  t(connections)[upper.tri(connections)]
  # order the nodes and set lower tri to FALSE
  connections = connections[order(ranks), order(ranks)]
  connections[lower.tri(connections)] = FALSE
  connections = connections[order(rownames(connections)), order(colnames(connections))]
  
  # Transitivity
  mode(connections) = "double"
  connections = useTransitivity(connections, searchForTransitivity)
  
  # Build the igraph edges vector
  edges = character(0)
  for (j in seq_len(ncol(connections))) {
    for (i in seq_len(nrow(connections))) {
      if (connections[i, j] == 1L)
        edges = c(edges, rownames(connections)[i], colnames(connections)[j])
    }
  }
  
  # Get lowest beaten test niveau for each edge
  niveaus = Reduce(`+`, lapply(test.niveaus, function(alpha) post.hoc < alpha))
  rownames(niveaus) = colnames(niveaus) = algo.names
  niveaus[upper.tri(niveaus)] =  t(niveaus)[upper.tri(niveaus)]
  niveaus = niveaus[order(rownames(niveaus)), order(colnames(niveaus))]
  line.types = niveaus[connections == 1]
  
  list(edges = edges, line.types = line.types)
}

