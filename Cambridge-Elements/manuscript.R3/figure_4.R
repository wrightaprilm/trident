norm1 <- rlnorm(n = 10000, sdlog = 1, mean = 1)
norm05 <- rlnorm(n = 10000, sdlog = .05, mean = 1)
norm5 <- rlnorm(n = 10000,  sdlog  = 5, mean = 1)
norm10 <- rlnorm(n = 10000, sdlog = 10, mean = 1)
norm15 <- rlnorm(n = 10000, sdlog = 15, mean = 1)

norm <- data.frame(norm1)
norm["Variance"] = "1"
colnames(norm) = c("Draw", "Variance")
norm05 <- data.frame(norm05)
norm05["Variance"] = ".05"
colnames(norm05) = c("Draw", "Variance")
norm5 <- data.frame(norm5)
norm5["Variance"] = "5"
colnames(norm5) <- c("Draw", "Variance")
norm10 <- data.frame(norm10)
norm10["Variance"] = "10"
colnames(norm10) <- c("Draw", "Variance")
norm15 <- data.frame(norm15)
norm15["Variance"] = "15"
colnames(norm15) <- c("Draw", "Variance")

f<- rbind(norm05, norm, norm10, norm15)
d<-f[!(f$`Draw`>4.5),]
lnorm <- ggplot(d, aes(x = Draw, y=`Variance`, fill=stat(x))) +
  ylab("Variance") +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low = "darkblue", high = "orange",name = "Value drawn from \n Lognormal \n distribution")  +
  theme_bw(base_size = 22) +
  xlab("Value drawn from \n Lognormal distribution") +
  theme(legend.position = "none")
