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

saberIt = function(data, perfName, expParName, algoName, replName,
  clusterFunction = "clusterFunctionHclust",
  idx = "dunn",
  minInCluster = length(levels(data$.expID)) * 0.1,
  max.nc = 5
) {
  
  assertDataFrame(data)
  
  # perforamce, replication and algorithm mus be single column from data
  assertChoice(perfName, choices = names(data))
  assertChoice(replName, choices = names(data))
  assertChoice(algoName, choices = names(data))
  
  # experimental params can be multiple columns from data
  assertSubset(expParName, choices = names(data))
  
  # check for variable names to be unique
  if (perfName == algoName || algoName == replName || perfName == replName ||
      perfName %in% expParName || algoName %in% expParName || replName %in% expParName) {
    stop("perfName, algoName, replName and expParName must be distinct.")
  }
  
  assertChoice(clusterFunction, choices = c("clusterFunctionKMeans", "clusterFunctionHclust"))
  cluster = get(clusterFunction)
  
  # Design unique .expID's
  data$.expID = factor(apply(data[, expParName], 1, paste0, collapse = "_"))
  data$.expreplID = factor(apply(data[, c(".expID", replName)], 1, paste0, collapse = ";"))
  
  n.exps = nlevels(data$.expID)
  n.algos = nlevels(data[, algoName])
  
  # At first, perform tests for complete data 
  global = friedman.test(y = data[, perfName], groups = data[, algoName],
    blocks = data[, ".expreplID"])$p.value
  nemenyi = pairwiseNemenyiTests(y = data[, perfName], groups =  data[, algoName],
    blocks = data[, ".expreplID"])
  
  complete.data = list(
    global = global,
    post.hoc = nemenyi$p.values,
    ranks = nemenyi$rank.means
  )
  
  # Build wide data.frame for clustering. Cluster based on ranks
  clust.data = lapply(split(data, data[, replName]), function(d)
    rmat = t(calculateRankMatrix(d[, perfName], d[, algoName], d[, ".expID"]))
  )
  clust.data = do.call(cbind, clust.data)
  
  
  # Do the clustering
  # Rule: Every cluster must consist of at least minInCluster observations
  # If this does not hold, the maximum number of allowed cluster is decreased
  # TODO: this is inefficient?
  repeat{
    clusters = cluster(clust.data = clust.data, 
      distMethod = "euclidean", 
      clusterMethod = "ward.D",
      idx = idx,
      max.nc = max.nc)
    max.nc = max.nc - 1
    inCluster = min(table(clusters$.clustID))
    if(inCluster >= minInCluster || max.nc == 0)
      break
  }
  
  # Split the data set into the clusters
  data = merge(data, clusters)
  data.clusterd = split(data, data$.clustID)
  
  # Perform global test in each cluster
  global = lapply(data.clusterd, function(d) {
    friedman.test(y = data[, perfName], groups = data[, algoName], blocks = data[, ".expreplID"])$p.value
  })
  
  # Perform pairwise tests in each cluster
  post.hoc = lapply(data.clusterd, function(d) {
    pairwiseNemenyiTests(y = d[, perfName], groups =  d[, algoName], blocks = d[, ".expreplID"])
  })
  
  # Build the result object
  structure(list(
    test.complete.data = complete.data,
    test.clusters = list(
      global = global,
      post.hoc = BBmisc::extractSubList(post.hoc, "p.values", simplify = FALSE),
      ranks = t(BBmisc::extractSubList(post.hoc, "rank.means", simplify = TRUE))
    ),
    clusters = clusters,
    pars = list(
      data = data,
      perfName = perfName,
      expParName = expParName,
      algoName = algoName,
      replName = replName
    )
  ), class = "saber")
}
