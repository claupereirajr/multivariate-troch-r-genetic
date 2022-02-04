#
##
pacotes <- c("biotools","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
#
##
algodao<-read_excel('algodao.xlsx',sheet = 'NND2M')
algodao <- read_excel("algodao.xlsx", sheet = "AVG")

df <- as.matrix(algodao[,3:6])
df

# manova
mod <- manova(df~as.factor(Genotype)+as.factor(Repl), data=algodao)
summary(mod)

ss <- SSD(mod)
ss

covar <- ss$SSD/ss$df
covar

# distribution
imp <- singh(df, covar)
imp

# plot
plot(imp)

# distance
d <-  D2.dist(avg[,-1], covar)

# tocher
toc <- tocher(d)
toc$clusters
toc$distClust

