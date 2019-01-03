# performs pairwise Nemenyi post-hoc tests

# Gets 3 vector of same length:
#  y - numeric variable, performance values
#  groups - factor variable giving the grouping
#  blocks - factor vector, the blocks

# returns a list with 2 elements:
#  - pstat, a table with pairwise p-value as returned by pairwise.table
#  - vector of mean ranks for each grop

pairwiseNemenyiTests = function(y, groups, blocks) {
  
  r = calculateRankMatrix(y, groups, blocks)
  k = nrow(r)
  N = ncol(r)
  rank.means = rowMeans(r)
  
  compareStats = function(i, j) {
    dif = abs(rank.means[i] - rank.means[j])
    stat = dif / sqrt(k * (k + 1) / (12 * N))
    pval = 1 - ptukey(stat, k, Inf)
    return(pval)
  }
  
  pstat = pairwise.table(compareStats, levels(groups), p.adjust.method = "none")
  
  list(p.values = pstat, rank.means = rank.means)
}
