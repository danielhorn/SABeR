pairwiseNemenyiTests <- function(y, groups, blocks) {
  
  # Preprocess inputs to get nice order
  groups <- factor(groups)
  blocks <- factor(blocks)
  o <- order(groups, blocks)
  y <- y[o]
  groups <- groups[o]
  blocks <- blocks[o]
  k <- nlevels(groups)
  N <- nlevels(blocks)
    
  # calculate ranks in each block
  y <- matrix(unlist(split(c(y), blocks)), ncol = k, byrow = TRUE)
  r <- apply(y, 1L, rank)
  row.names(r) <- levels(groups)
  colnames(r) <- levels(blocks)
  rank.means <- rowMeans(r)
  
  compareStats <- function(i, j) {
    dif <- abs(rank.means[i] - rank.means[j])
    stat <- dif / sqrt(k * (k + 1) / (12 * N))
    pval <- 1 - ptukey(stat, k, Inf)
    return(pval)
  }
  
  pstat <- pairwise.table(compareStats, levels(groups), p.adjust.method = "none")
  
  list(statistic = pstat, rank.means = rank.means)
  
}