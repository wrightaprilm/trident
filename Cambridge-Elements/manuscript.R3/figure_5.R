
source("reconstructed.tree.R") # functions used to generate reconstructed tree and corresponding fossils objects

set.seed(123)

## pure birth tree

birth = 0.1
death = 0

tips = 10

trees = TreeSim::sim.bd.taxa(10, 10, birth, death)

t1 = trees[[9]]

# trim the root edge
t1$root.edge = t1$root.edge - 8

f1 = FossilSim::sim.extant.samples(FossilSim::fossils(), t1)

pdf("tree1a.pdf", width = 5, height = 4)
plot(f1, t1, edge.width = 1.3, cex = 1.3, max.age = 40) # complete tree
dev.off()

pdf("tree1b.pdf", width = 5, height = 4)
plot(f1, t1, edge.width = 1.3, cex = 1.3, hide.edge = TRUE, max.age = 40) # reconstructed tree
dev.off()

## birth death

death = 0.05

trees = TreeSim::sim.bd.taxa(10, 10, birth, death)

t2 = trees[[2]]

f2 = FossilSim::sim.extant.samples(FossilSim::fossils(), t2)

t2$root.edge = 5

pdf("tree2a.pdf", width = 5, height = 4)
plot(f2, t2, edge.width = 1.3, cex = 1.3, max.age = 40) # complete tree
dev.off()

t3 = geiger::drop.extinct(t2)

pdf("tree2b.pdf", width = 5, height = 4)
plot(f2, t3, edge.width = 1.3, cex = 1.3, hide.edge = TRUE, max.age = 40) # reconstructed tree
dev.off()

## birth death rho sampling

f3 = FossilSim::sim.extant.samples(FossilSim::fossils(), t2, rho = 0.6)

pdf("tree3a.pdf", width = 5, height = 4)
plot(f3, t2, edge.width = 1.3, cex = 1.3, max.age = 40) # complete tree
dev.off()

# drop unsampled extant taxa
unsamp = c()
for(i in t3$tip.label){
  if(!(which(t3$tip.label == i) %in% f3$edge)) unsamp = c(unsamp, i)
}

t4 = ape::drop.tip(t3, unsamp)

f4 = FossilSim::sim.extant.samples(FossilSim::fossils(), t4)

pdf("tree3b.pdf", width = 5, height = 4)
plot(f4, t4, edge.width = 1.3, cex = 1.3, hide.edge = TRUE, max.age = 40) # reconstructed tree
dev.off()

# fbd

set.seed(1234)

f4 = FossilSim::sim.fossils.poisson(tree = t2, rate = 0.05)

# sneak in a stem fossil
h = runif(1, max(n.ages(t2)), 37)
j = length(t2$tip.label) + 1
f4 = rbind(f4, data.frame(sp = j, edge = j, hmin = h, hmax = h))

# add sampled extant tips from previous example

f5 = f4
samp.tips = t4$tip.label

for(i in samp.tips){
  j = which(t2$tip.label == i)
  f5 = rbind(f5, data.frame(sp = j, edge = j, hmin = 0, hmax = 0))
}

pdf("tree4a.pdf", width = 5, height = 4)
plot(f5, t2, edge.width = 1.3, cex = 1.3, max.age = 40) # complete tree
dev.off()

out = reconstructed.tree.fossils.objects(f4, t2, samp.tips)

f5 = out$fossils
t5 = out$tree

pdf("tree4b.pdf", width = 5, height = 4)
plot(f5, t5, edge.width = 1.3, cex = 1.3, max.age = 40) # reconstructed tree
dev.off()

# fbd range


