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
#' @param method [\code{character(1)}] \cr
#'   Either parametric (\code(p)) or non-parametric (\code{np}, the default)
#' @param clusterFunction [\code{character(1)}] \cr
#'   Name of the cluster function to be used. At the moment, only 
#'   clusterFunctionHclust is supported
#' @return A \code{saber}, object.
#' 
#' @export

saberIt = function(data, perfName, expParName, algoName, replName,
  method = "np",
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
  
  #
  if (perfName == algoName || algoName == replName || perfName == replName ||
      perfName %in% expParName || algoName %in% expParName || replName %in% expParName) {
    stop("perfName, algoName, replName and expParName must be distinct.")
  }
  
  assertChoice(clusterFunction, choices = c("clusterFunctionKMeans", "clusterFunctionHclust"))
  cluster = get(clusterFunction)
  
  
  assertChoice(method, choices = c("p", "np"))
  
  # Design unique .expID's
  data$.expID = factor(apply(data[, expParName], 1, paste0, collapse = "_"))
  n.exps = nlevels(data$.expID)
  n.algos = nlevels(data[, algoName])
  
  data$.expreplID = factor(apply(data[, c(".expID", replName)], 1, paste0, collapse = ";"))
  
  # At first, perform tests for complete data 
  # Parametric: ANOVA and Tukey
  if (method == "p") {
    formula = reformulate(response = perfName, termlabels = c(algoName, replName, expParName))
    mod = aov(formula, data = data)
    # TODO: is there a better way to get this p values?
    global = summary(mod)[[1]][[5]][1]
    # TODO: nasty way to get this in a nisc format
    p.values = matrix(nrow = n.algos, ncol = n.algos)
    p.values[lower.tri(p.values)] = TukeyHSD(mod, which = algoName)[[1]][, 4]
    post.hoc = pairwise.table(function(i, j) p.values[i, j], levels(data[, algoName]),
      p.adjust.method = "none")
    sort(tapply(data[, perfName], data[, algoName], mean))
  }
  
  # Non-Parametric: Friedmann and Nemenyi
  if (method == "np") {
    global = friedman.test(y = data[, perfName], groups = data[, algoName],
      blocks = data[, ".expreplID"])$p.value
    nemenyi = pairwiseNemenyiTests(y = data[, perfName], groups =  data[, algoName],
      blocks = data[, ".expreplID"])
    post.hoc = nemenyi$p.values
    perf = nemenyi$rank.means
  }
  
  complete.data = list(global = global, post.hoc = post.hoc, perf = perf)
  
  # Build wide data.frame for clustering
  
  # Parametric: Cluster based on performance values 
  if (method == "p") {
    clust.data = data.frame(data[, c(".expID", algoName, replName, perfName)]) 
    clust.data$colid = paste(clust.data[, algoName], clust.data[, replName], sep = "_")
    clust.data = reshape2::dcast(clust.data, .expID ~ colid, value.var = "ydist")
    rownames(clust.data) = clust.data$.expID
    clust.data = clust.data[, -1]
  }
  
  # Non-Parametric: Cluster based on ranks
  if (method == "np") {
    clust.data = lapply(split(data, data[, replName]), function(d)
      rmat = t(calculateRankMatrix(d[, perfName], d[, algoName], d[, ".expID"]))
    )
    clust.data = do.call(cbind, clust.data)
  }
  
  
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
  
  # Cluster-Mittelpunkte
  cluster.mittel = sapply(data.clusterd, function(d) colMeans(d[, expParName]))
  
  # Perform pairwise tests in each cluster
  if (method == "p") {
    ## TukeyHSD
    stop("Not implemented yet")
  }
  if (method == "np") {
    test.results = lapply(data.clusterd, function(d) {
      d$.exp.repl.id = apply(d[, c(".expID", replName)], 1, paste, collapse = "_")
      pairwiseNemenyiTests(y = d[, perfName], groups =  d[, algoName],
        blocks = d[, ".exp.repl.id"])
    })
  }
  
  result = structure(list(
    test.results = BBmisc::extractSubList(test.results, "statistic", simplify = FALSE),
    rank.matrix = t(BBmisc::extractSubList(test.results, "rank.means", simplify = TRUE)),
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
