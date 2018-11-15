#' @title Statistical Analysis of Benchmark Results
#'
#' @description
#' TODOTODOTODOTODOTODOTODO
#'
#' @param data [\code{data.frame}] \cr
#'   Data.frame containing the data to be analyzed
#' @param perfName [\code{character(1)}] \cr
#'   Name of the column of data containing the performance values
#' @param expParName [\code{character()}] \cr
#'   Names of the columns of data containing the experiment parameters
#' @param algoName [\code{character(1)}] \cr
#'   Name of the column of data containing the used algorithm
#' @param replName [\code{character(1)}] \cr
#'   Name of the column of data containing the replication number
#' @param clusterFunction [\code{character(1)}] \cr
#'   Name of the cluster function to be used. At the moment, only 
#'   clusterFunctionHclust is supported
#' @return A \code{saber}, object.
#' 
#' @export

saberIt <- function(data, perfName, expParName, algoName, replName, clusterFunction,
                    idx = "dunn", minInCluster = length(levels(data$.expID)) * 0.1,
                    max.nc = 5) {
  assert_string(perfName)
  assert_string(replName)
  assert_string(algoName)
  assert_character(expParName)
  assert_choice(clusterFunction, choices = c("clusterFunctionKMeans", "clusterFunctionHclust"))
  cluster <- get(clusterFunction)
  assert_data_frame(data)
  
  # Design unique .expID's
  data$.expID <- factor(apply(data[, expParName], 1, paste, collapse = "_"))
  Nlevel <- length(levels(data$.expID))
  
  # Build wide data.frame for clustering
  clust.data <- data.frame(data[, c(".expID", algoName, replName, perfName)]) 
  clust.data$colid <- paste(clust.data[, algoName], clust.data[, replName], sep = "_")
  clust.data <- dcast(clust.data, .expID ~ colid, value.var = "ydist")
  rownames(clust.data) <- clust.data$.expID
  clust.data <- clust.data[, -1]
  
  # Do the clustering
  #n <- 
  repeat{
  clusters <- cluster(clust.data = clust.data, 
                      distMethod = "euclidean", 
                      clusterMethod = "ward.D",
                      idx = idx,
                      max.nc = max.nc)
  max.nc <- max.nc - 1
  inCluster <- min(table(clusters$.clustID))
  if(inCluster >= minInCluster || max.nc == 0)
    break
  }
  # Split the data set into the clusters
  data <- merge(data, clusters)
  data.clusterd <- split(data, data$.clustID)
  
  # Cluster-Mittelpunkte
  cluster.mittel <- sapply(data.clusterd, function(d) colMeans(d[, expParName]))
  
  # Paarweise Tests
  test.results <- pairwiseTests(data.clusterd, perfName = perfName,
    algoName = algoName, replName = replName )
  
  result <- structure(list(
    test.results = test.results$p.values,
    rank.matrix = t(test.results$rank.matrix),
    clusters = clusters,
    cluster.mittel = cluster.mittel,
    cluster.sizes = table(clusters$.clustID),
    pars = list(
      data = data,
      algoName = algoName
    )
  ), class = "saber")
  
  return(result)
}
