library(phytools)
library(ggtree)

set.seed(124)

tree <- pbtree(b = 1, d = .5, n=5)
tree <- pbtree(b = 1, d = .5, n=5)
ggplot(tree, aes(x, y)) + geom_tree() + theme_tree()  + geom_nodepoint(color="grey", alpha=4/5, size=5, shape="triangle") + geom_tippoint(color="grey", alpha=4/5, size=5) 

age_df <- read.csv("ages", sep = "\t", row.names = "taxon")
scaley <- paleotree::cal3TimePaleoPhy(tree, age_df, sampRate = .0001, brRate=1, extRate = .5)

strap::geoscalePhylo(scaley, age_df, units = c("Epoch", "Era"), tick.scale = 10, cex.ts = .5, cex.age = .5)
