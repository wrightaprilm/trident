
# Identify parent node
ancestor = function(edge,tree){
  parent = tree$edge[,1][which(tree$edge[,2]==edge)]
  return(parent)
}

# calculate node ages
n.ages = function(tree){

  depth = ape::node.depth.edgelength(tree)
  node.ages = max(depth) - depth
  names(node.ages) <- 1:(tree$Nnode+length(tree$tip))

  # adding possible offset if tree fully extinct
  if(!is.null(tree$root.time)) node.ages = node.ages + tree$root.time - max(node.ages)

  return(node.ages)
}

# return tip.labels of extinct tips
is.extinct = function(phy, tol=NULL){
  if (!"phylo" %in% class(phy)) {
    stop("\"phy\" is not of class \"phylo\".");
  }
  if (is.null(phy$edge.length)) {
    stop("\"phy\" does not have branch lengths.");
  }
  if (is.null(tol)) {
    tol <- min(phy$edge.length)/100;
  }
  Ntip <- length(phy$tip.label)
  phy <- ape::reorder.phylo(phy);
  xx <- numeric(Ntip + phy$Nnode);
  for (i in 1:length(phy$edge[,1])) {
    xx[phy$edge[i,2]] <- xx[phy$edge[i,1]] + phy$edge.length[i];
  }
  aa <- max(xx[1:Ntip]) - xx[1:Ntip] > tol;
  if (any(aa)) {
    return(phy$tip.label[which(aa)]);
  } else {
    return(NULL);
  }
}

# function to generate tree and corresponding fossil object for the reconstructed tree
reconstructed.tree.fossils.objects = function(fossils, tree, sampled_tips){

  # 1. create SAtree object
  sa.tree = FossilSim::SAtree.from.fossils(tree, fossils)
  # the following function should be able to take care of unsampled tips but isn't working for some reason
  samp.tree = FossilSim::sampled.tree.from.combined(sa.tree)
  # find unsampled extant tips
  ext = is.extinct(sa.tree, tol = 1e-8)
  unsampled_tips = c()
  for(i in sa.tree$tip.label){
    if(i %in% ext) next
    x = strsplit(i,"_")[[1]][1]
    if(!x %in% sampled_tips) unsampled_tips = c(unsampled_tips, i)
  }
  # drop unsampled tips
  samp.tree = ape::drop.tip(samp.tree, unsampled_tips)

  # 2. identify sampled ancestors and drop from the sampled tree
  sa = c()
  for(i in 1:length(samp.tree$tip.label)){
    blength = samp.tree$edge.length[which(samp.tree$edge[,2]==i)]
    if(abs(blength) < 1e-8) sa = c(sa, samp.tree$tip.label[i])
  }
  no.sa.tree = ape::drop.tip(samp.tree, sa)

  # 3. create new fossils object based on the reconstructed tree
  # 3a: deal with sampled ancestors
  f.new = data.frame()
  for(i in sa){
    anc = ancestor(which(samp.tree$tip.label==i), samp.tree)

    s1 = ape::extract.clade(samp.tree,anc)$tip.label
    s2 = no.sa.tree$tip.label[which(no.sa.tree$tip.label %in% s1)]

    # assign fossils to nearest descendent node in the tree
    if(length(s2) > 1){
      j = ape::getMRCA(no.sa.tree,s2)
    } else { # i is SA on terminal branch
      j = which(no.sa.tree$tip.label==s2)
    }
    h = n.ages(samp.tree)[[which(samp.tree$tip.label==i)]] # some redundancy here, could calculate once, might be faster for large trees
    f.new = rbind(f.new, data.frame(sp = j,
                                    edge = j,
                                    hmin = h,
                                    hmax = h))
  }
  f.new = FossilSim::fossils(f.new)

  # 3b. deal with non SA samples
  f.new = FossilSim::sim.tip.samples(f.new, no.sa.tree)

  #4. add a root edge for any stem fossils
  if( any(f.new$edge == (length(no.sa.tree$tip.label)+1) )){
    h = f.new$hmin[which(f.new$edge == (length(no.sa.tree$tip.label)+1))]
    no.sa.tree$root.edge = h - max(n.ages(no.sa.tree))
  }

  return(list(tree = no.sa.tree, fossils = f.new))

}
