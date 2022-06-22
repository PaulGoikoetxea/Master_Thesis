
#########################################################################

load("ret_end_ts.RData")

#########################################################################
# Load packages

pacman::p_load(rugarch, moments, fGarch, xts, copula, VineCopula, ggplot2,
               factoextra,dendextend, cluster, GGally, ggfortify,
               gridExtra)

#########################################################################
# Numerical summary of the returns

mean_ret_ts = colMeans(ret_ts)
median_ret_ts = apply(ret_ts,2,median)
sd_ret_ts = apply(ret_ts,2,sd)
skew_ret_ts = apply(ret_ts,2,skewness)
kurt_ret_ts = apply(ret_ts,2,kurtosis)

mmsk_ret_ts = cbind(mean_ret_ts, median_ret_ts, sd_ret_ts, skew_ret_ts, kurt_ret_ts)
colnames(mmsk_ret_ts) = c("Mean", "Median", "Standard Deviantion", "Skewness","Kurtosis")
head(mmsk_ret_ts)

#Tables and graphs that represent the presence of extreme values

ggplot(prices_ts, aes(y=BTC.USD)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_minimal() +
  ggtitle("Maximum temperature per day") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 

# Prices
g1 = autoplot(prices_ts[, 1], ts.colour = 'green') + theme_minimal() +
  xlab("Time") + ylab("BTC.USD price") + ggtitle("BTC-USD Price Adjusted")

g2 = autoplot(prices_ts[, 2], ts.colour = 'blue') + theme_minimal() +
  xlab("Time") + ylab("ETH.USD price") + ggtitle("ETH-USD Price Adjusted")

grid.arrange(g1, g2, nrow = 2)
# Returns
g1 = autoplot(ret_ts[, 1], ts.colour = 'green') + theme_minimal() +
  xlab("Time") + ylab("BTC.USD price") + ggtitle("BTC-USD log return")

g2 = autoplot(ret_ts[, 2], ts.colour = 'blue') + theme_minimal() +
  xlab("Time") + ylab("ETH.USD price") + ggtitle("ETH-USD log return")

grid.arrange(g1, g2, nrow = 2)


#########################################################################
# Obtain standardized residual series and the associated probabilities

Spec <- ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0),include.mean=FALSE),
                   distribution.model="sged")

prob_ts = matrix(0,nrow(ret_ts),ncol(ret_ts))
colnames(prob_ts) = colnames(ret_ts)


for(i in 1:ncol(ret_ts)){
  print(i)
  fit = ugarchfit(data = ret_ts[, i], spec = Spec, solver = "hybrid")
  res_col = as.matrix(residuals(fit, standardize = T))
  prob_ts[, i] = psged(res_col[, 1])
  rm(fit)
}

save(prob_ts,file="prob_ts.RData")

#########################################################################
# Compute the lower tail dependence after selecting an appropriate copula

for(i in 1:137){
  for(j in 1:137){
    if(i < j){
      print(c(i,j))
      m = as.matrix(prob_ts[, c(i, j)])
      fit = selectedCopula = BiCopSelect(m[, 1], m[, 2], familyset = NA)
      ltd2[i, j] = as.numeric(fit$taildep[1])
      rm(fit, m)
    }
  }
  save(ltd2,file="lower_tail_mat_PG.RData")
}

load("lower_tail_mat_PG.RData")

#########################################################################
# Here we have a problem because there are many lower tail dependences that are exactly equal to 0. 
# Possibly this is due to the fact that the selected copula is the Uniform or the Gaussian, which 
# have lower tail dependence 0. When doing the transformation with -log(), the 0s become Inf distances. 
#########################################################################

#########################################################################
# 1. We are going to eliminate from the analysis those cryptocurrencies whose maximum tail dependence 
# with the rest of the cryptocurrencies is less than 0.1. This allows us to eliminate 28 cryptocurrencies 
# that behave independently of the rest in terms of risk, so they are kind of outliers

ltd2.sim <- ltd2
ltd2.sim[lower.tri(ltd2.sim)]  <- t(ltd2.sim)[lower.tri(ltd2.sim)]

elim <- which(apply(ltd2.sim,2,max)<0.10)
length(elim)

ltd2.new <- ltd2.sim[-elim,-elim]

#########################################################################
# 2. Obtain the distance matrix and replace Infs with appropriate values

# Put the values of the upper part of ltd2.new in a vector

ltd2.new.vec <- ltd2.new[upper.tri(ltd2.new,diag=FALSE)]

# Make the transformation

neg.log.ltd2.new.vec <- -log(ltd2.new.vec)

# Extract those values that are finite

finite.vec <- neg.log.ltd2.new.vec[is.finite(neg.log.ltd2.new.vec)]

# How many Infs?

n.Inf <- length(neg.log.ltd2.new.vec) - length(finite.vec)

# Replace the Infs with traslated exponentials that provide greater distances than the existing ones

neg.log.ltd2.new.vec[is.infinite(neg.log.ltd2.new.vec)] <- 
  max(finite.vec) + rexp(n.Inf,rate=1/sd(finite.vec))

# Define the matrix DeltaMat

DeltaMat <- matrix(0,nrow=nrow(ltd2.new),ncol=ncol(ltd2.new))
DeltaMat[upper.tri(DeltaMat,diag=FALSE)] <- neg.log.ltd2.new.vec

#########################################################################
# Get the symmetric matrix

DeltaMat[lower.tri(DeltaMat)]  <- t(DeltaMat)[lower.tri(DeltaMat)]

# Rename columns and rows of DeltaMat

colnames(DeltaMat) = colnames(ltd2.new)
rownames(DeltaMat) = rownames(ltd2.new)

#########################################################################
# Get the distance matrix

Delta_dist = as.dist(DeltaMat)

#########################################################################
# Hierarchical clustering
#########################################################################

#########################################################################
# Agglomerative hierarchical clustering

# Run single linkage

single.hc <- hclust(d=Delta_dist,method="single")
fviz_dend(single.hc,cex=0.5)

# Run complete linkage

complete.hc <- hclust(d=Delta_dist,method="complete")
fviz_dend(complete.hc,cex=0.5)

# Run average linkage

average.hc <- hclust(d=Delta_dist,method="average")
fviz_dend(average.hc,cex=0.5)

# Run ward linkage

ward.hc <- hclust(d=Delta_dist,method="ward.D2")
fviz_dend(ward.hc,cex=0.5)

#########################################################################

# Number of k?

fviz_nbclust(DeltaMat,hcut , method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")

silhouette_score <- function(k){
  single.hc <- hclust(d=Delta_dist,method="single")
  ss <- silhouette(cutree(single.hc, k = k), Delta_dist)
  mean(ss[, 3])
}
avg_sil1 <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil1, xlab='Number of clusters', ylab='Average Silhouette Scores for single linkage', frame=FALSE)



silhouette_score <- function(k){
  complete.hc <- hclust(d=Delta_dist,method="complete")
  ss <- silhouette(cutree(complete.hc, k = k), Delta_dist)
  mean(ss[, 3])
}
avg_sil2 <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil2, xlab='Number of clusters', ylab='Average Silhouette Scores for complete linkage', frame=FALSE)



silhouette_score <- function(k){
  average.hc <- hclust(d=Delta_dist,method="average")
  ss <- silhouette(cutree(average.hc, k = k), Delta_dist)
  mean(ss[, 3])
}
avg_sil3 <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil3, xlab='Number of clusters', ylab='Average Silhouette Scores for average linkage', frame=FALSE)



silhouette_score <- function(k){
  ward.hc <- hclust(d=Delta_dist,method="ward.D2")
  ss <- silhouette(cutree(ward.hc, k = k), Delta_dist)
  mean(ss[, 3])
}
k <- 2:6
avg_sil4 <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil4, xlab='Number of clusters', ylab='Average Silhouette Scores for ward linkage', frame=FALSE)


# Ward's linkage appears to be the more reasonable choice

fviz_dend(ward.hc, k = 2, # Cut in two groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

# Silhouette plot for k = 2

plot(silhouette(cutree(ward.hc, k = 2)))


fviz_dend(ward.hc, k = 3, # Cut in three groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_dend(ward.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

fviz_dend(ward.hc, k = 5, # Cut in five groups
          cex = 0.5, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

############################################################################

# Portfolio

ret_ts_port = ret_ts[, -elim]

# Final return series ggpairs plot

mean_ret_ts = colMeans(ret_ts_port)
median_ret_ts = apply(ret_ts_port,2,median)
sd_ret_ts = apply(ret_ts_port,2,sd)
skew_ret_ts = apply(ret_ts_port,2,skewness)
kurt_ret_ts = apply(ret_ts_port,2,kurtosis)

mmsk_ret_ts = cbind(mean_ret_ts, median_ret_ts, sd_ret_ts, skew_ret_ts, kurt_ret_ts)
colnames(mmsk_ret_ts) = c("Mean", "Median", "Standard Deviantion", "Skewness","Kurtosis")

ggpairs(as.data.frame(mmsk_ret_ts))


# Groups k = 5, ward linkage

port1 = ret_ts_port[, which(cutree(ward.hc,k=5)==1)]
port2 = ret_ts_port[, which(cutree(ward.hc,k=5)==2)]
port3 = ret_ts_port[, which(cutree(ward.hc,k=5)==3)]
port4 = ret_ts_port[, which(cutree(ward.hc,k=5)==4)]
port5 = ret_ts_port[, which(cutree(ward.hc,k=5)==5)]

p_all = matrix(0,nrow=31,ncol=10000)
p_all2 = matrix(0,nrow=31,ncol=10000)

unos = matrix(1,nrow=5,ncol=1)

for(i in 1:10000){
  
  ret1_all = port1[,sample(dim(port1)[2],1)]
  ret2_all = port2[,sample(dim(port2)[2],1)]
  ret3_all = port3[,sample(dim(port3)[2],1)]
  ret4_all = port4[,sample(dim(port4)[2],1)]
  ret5_all = port5[,sample(dim(port5)[2],1)]
  
  # remove observations of December 2021 
  ret1 = ret1_all[-c(1430:1460),] 
  ret2 = ret2_all[-c(1430:1460),]
  ret3 = ret3_all[-c(1430:1460),] 
  ret4 = ret4_all[-c(1430:1460),]
  ret5 = ret5_all[-c(1430:1460),]
  
  cov_mat = cov(cbind(ret1,ret2,ret3,ret4,ret5))
  inv_cov_mat = solve(cov_mat)
  c = 1 / as.numeric(t(unos)%*%inv_cov_mat%*%unos) * inv_cov_mat %*% unos
  
  p_all[,i] = c[1] * ret1_all[c(1430:1460),] + c[2] * ret2_all[c(1430:1460),] + c[3] * ret3_all[c(1430:1460),] +
    c[4] * ret4_all[c(1430:1460),] + c[5] * ret5_all[c(1430:1460),]
  
}

# Without groups
for(i in 1:10000){
  
  # remove observations of December 2021 
  ret_all = ret_ts_port[, sample(dim(ret_ts_port)[2],5)]
  ret = ret_all[-c(1430:1460), ]
  
  cov_mat = cov(ret)
  inv_cov_mat = solve(cov_mat)
  c = 1 / as.numeric(t(unos)%*%inv_cov_mat%*%unos) * inv_cov_mat %*% unos
  
  p_all2[,i] = c[1] * ret_all[c(1430:1460),1] + c[2] * ret_all[c(1430:1460),2] + c[3] * ret_all[c(1430:1460),3] +
    c[4] * ret_all[c(1430:1460),4] + c[5] * ret_all[c(1430:1460),5]
  
}

par(mfrow=c(1,1))
matplot(as.data.frame(p_all),type="l",xlab="December 2021",ylab="Portfolio returns by groups",col="blue")
lines(colMeans(t(p_all2)),col="red",lwd=2)

# Groups k = 2, ward linkage

port1 = ret_ts_port[, which(cutree(ward.hc,k=2)==1)]
port2 = ret_ts_port[, which(cutree(ward.hc,k=2)==2)]

p_all = matrix(0,nrow=31,ncol=10000)
p_all2 = matrix(0,nrow=31,ncol=10000)

unos = matrix(1,nrow=2,ncol=1)

for(i in 1:10000){
  
  ret1_all = port1[,sample(dim(port1)[2],1)]
  ret2_all = port2[,sample(dim(port2)[2],1)]
  
  # remove observations of December 2021 
  ret1 = ret1_all[-c(1430:1460),] 
  ret2 = ret2_all[-c(1430:1460),]
  
  
  cov_mat = cov(cbind(ret1,ret2))
  inv_cov_mat = solve(cov_mat)
  c = 1 / as.numeric(t(unos)%*%inv_cov_mat%*%unos) * inv_cov_mat %*% unos
  
  p_all[,i] = c[1] * ret1_all[c(1430:1460),] + c[2] * ret2_all[c(1430:1460),] 
  
}

# Without groups
for(i in 1:10000){
  
  # remove observations of December 2021 
  ret_all = ret_ts_port[, sample(dim(ret_ts_port)[2],2)]
  ret = ret_all[-c(1430:1460), ]
  
  cov_mat = cov(ret)
  inv_cov_mat = solve(cov_mat)
  c = 1 / as.numeric(t(unos)%*%inv_cov_mat%*%unos) * inv_cov_mat %*% unos
  
  p_all2[,i] = c[1] * ret_all[c(1430:1460),1] + c[2] * ret_all[c(1430:1460),2] 
  
}

par(mfrow=c(1,1))
matplot(as.data.frame(p_all),type="l",xlab="December 2021",ylab="Portfolio returns by groups",col="blue")
lines(colMeans(t(p_all2)),col="red",lwd=2)
