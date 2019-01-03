
# Gets 3 vector of same length:
#  y - numeric variable, performance values
#  groups - factor variable giving the grouping
#  blocks - factor vector, the blocks

# In each block, the groups are ordered, i.e. ranks between 1 and nlevels(blocks)
# are given. A matrix of dimension (nlevels(groups), nlevel(blocks)) is returned

calculateRankMatrix = function(y, groups, blocks) {
  
  # Preprocess inputs to get data into a well-defined order
  groups = factor(groups)
  blocks = factor(blocks)
  o = order(groups, blocks)
  y = y[o]
  groups = groups[o]
  blocks = blocks[o]
  k = nlevels(groups)
  N = nlevels(blocks)
  
  # calculate ranks in each block
  y = matrix(unlist(split(c(y), blocks)), ncol = k, byrow = TRUE)
  r = apply(y, 1L, rank)
  row.names(r) = levels(groups)
  colnames(r) = levels(blocks)
  
  return(r)
}
