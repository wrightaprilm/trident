exp1 <- rexp(n = 1000,rate = 1)
exp05 <- rexp(n = 1000,rate = .5)
exp5 <- rexp(n = 1000,rate = 5)
exp10 <- rexp(n = 1000,rate = 10)
exp15 <- rexp(n = 1000,rate = 15)

exp <- data.frame(exp1)
exp["Rate parameter"] = "1"
colnames(exp) = c("Value drawn from Exponentional distribution", "Rate Parameter")
exp05 <- data.frame(exp05)
exp05["Rate parameter"] = ".05"
colnames(exp05) = c("Value drawn from Exponentional distribution", "Rate Parameter")
exp5 <- data.frame(exp5)
exp5["Rate parameter"] = "5"
colnames(exp5) <- c("Value drawn from Exponentional distribution", "Rate Parameter")
exp10 <- data.frame(exp10)
exp10["Rate parameter"] = "10"
colnames(exp10) <- c("Value drawn from Exponentional distribution", "Rate Parameter")
exp15 <- data.frame(exp15)
exp15["Rate parameter"] = "15"
colnames(exp15) <- c("Value drawn from Exponentional distribution", "Rate Parameter")
f<- rbind(exp, exp05, exp10, exp15)
d<-f[!(f$`Value drawn from Exponentional distribution`>4.5),]
expplot <- ggplot(d, aes(x = `Value drawn from Exponentional distribution`, y=`Rate Parameter`, fill=stat(x))) +
 geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  xlab("Value drawn from \n Exponentional distribution") +
  ylab("Rate") +
  scale_fill_gradient(low = "darkblue", high = "orange",name = "Value drawn from \n Exponentional \n distribution")  +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")

norm1 <- rlnorm(n = 1000,sdlog = 1, mean = 1)
norm05 <- rlnorm(n = 1000,sdlog = .5, mean = 1)
norm5 <- rlnorm(n = 1000,sdlog  = 5, mean = 1)
norm10 <- rlnorm(n = 1000,sdlog = 10, mean = 1)
norm15 <- rlnorm(n = 1000,sdlog = 15, mean = 1)

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
f<- rbind(norm, norm05, norm10, norm15)
d<-f[!(f$`Draw`>4.5),]
lnorm <- ggplot(d, aes(x = `Draw`, y=`Variance`, fill=stat(x))) +
  ylab("Variance") +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_gradient(low = "darkblue", high = "orange",name = "Value drawn from \n Lognormal \n distribution")  +
  theme_bw(base_size = 22) +
  xlab("Value drawn from \n Lognormal distribution") +
  theme(legend.position = "none")


d1 <- rbeta(n = 1000, 1,1)
d05 <- rbeta(n = 1000, .05,.05)
d5 <- rbeta(n = 1000, 5,5)
d10 <- rbeta(n = 1000, 10,10)
d15 <- rbeta(n = 1000, 15,15)

d1 <- data.frame(d1)
d1["Shape"] = "1"
colnames(d1) = c("Draw", "Shape")
d05 <- data.frame(d05)
d05["Shape"] = ".05"
colnames(d05) = c("Draw", "Shape")
d15 <- data.frame(d15)
d15["Shape"] = "15"
colnames(d15) <- c("Draw", "Shape")
f<- rbind(d1, d05, d15)
dplot <- ggplot(f, aes(x =`Draw`, y=`Shape`, fill=stat(x))) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
  scale_fill_gradient(low = "darkblue", high = "orange", name = "Value drawn from \n Dirichlet \n distribution")  +
  theme_bw(base_size = 22) +
  xlab("Value drawn from \n Dirichlet distribution") +
  theme(legend.position = "none")

grid.arrange(expplot, lnorm, dplot, padding = 1)

