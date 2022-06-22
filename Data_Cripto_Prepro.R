
#########################################################################################################
# Remove all

rm(list=ls())

#########################################################################################################
# Load libraries

library(imputeTS)
library(quantmod)
library(moments)
library(xts)

#########################################################################################################
#########################################################################################################
# Now, we have the data saved. Let's check the missing data
#########################################################################################################
#########################################################################################################

#########################################################################################################
# Load the csv file as a data frame

setwd("G:\\Mi unidad\\MSDS\\TFM")
prices_df <- read.csv2("Price_close_Crypto.csv",sep=";")

#########################################################################################################
# How many NAs in each price series?

n_nas <- sapply(prices_df,function(x) sum(is.na(x)))
table(n_nas)

#########################################################################################################
# Thus, there are 137 price series with no NAs, 1 series with 2, 4, 8, 17, 18, 102, 225, 273, 280, 281, 
# 295, 298, 312, 330 and 338 NAs, and 2 series with 284 NAs

#########################################################################################################
# Identify the series with 2 NAs. These are isolated, so we interpolate

w_nas <- which(n_nas==2)
w_nas
t_nas <- which(is.na(prices_df[,which(n_nas==2)])==TRUE)
t_nas
prices_df[t_nas,w_nas] <- na_interpolation(prices_df[,w_nas],option="stine")[t_nas]

#########################################################################################################
# Identify the series with 4 NAs. These are the last four points, so we interpolate

w_nas <- which(n_nas==4)
w_nas
t_nas <- which(is.na(prices_df[,which(n_nas==4)])==TRUE)
t_nas
prices_df[t_nas,w_nas] <- na_interpolation(prices_df[,w_nas],option="stine")[t_nas]

#########################################################################################################
# Identify the series with 8 NAs. These are the last eight points. Too many, so this series will be discarded

w_nas <- which(n_nas==8)
w_nas
t_nas <- which(is.na(prices_df[,which(n_nas==8)])==TRUE)
t_nas

#########################################################################################################
# Identify the series with 17 NAs. The are 16 consecutive points. Too many, so this series will be discarded

w_nas <- which(n_nas==17)
w_nas
t_nas <- which(is.na(prices_df[,which(n_nas==17)])==TRUE)
t_nas

#########################################################################################################
# Identify the series with 18 NAs. The are 18 consecutive points. Too many, so this series will be discarded

w_nas <- which(n_nas==18)
w_nas
t_nas <- which(is.na(prices_df[,which(n_nas==18)])==TRUE)
t_nas

#########################################################################################################
# The series with more than 100 NAs will be discarded

col_elim <- which(colSums(is.na(prices_df)) > 0)
col_elim
prices_df_new <- prices_df[,-col_elim]

#########################################################################################################
# Check again how many NAs in each price series?

n_nas <- sapply(prices_df_new,function(x) sum(is.na(x)))
table(n_nas)

#########################################################################################################
# Now, we can define an xts object with dates

prices_ts <- xts(prices_df_new[,-1],as.Date(prices_df_new$Date)) 
dim(prices_ts)

#########################################################################################################
# Convert prices into returns

ret <- apply(prices_ts,2,Delt,type="log") 

# Eliminate the first row because it is NA

ret <- ret[-1,]
head(ret)

#########################################################################################################
# Convert returns into a xts object

ret_ts <- prices_ts[-1,]
ret_ts[1:nrow(ret_ts),1:ncol(ret_ts)] <- ret

#########################################################################################################
#########################################################################################################
# Now, we have the returns. Let's check inconsistencies in the return time series
# These inconsistencies may be due to errors in the data or a very strange behavior
#########################################################################################################
#########################################################################################################

#########################################################################################################
# There are 138 return series. Let's plot them and see problems
#########################################################################################################

par(mfrow=c(5,2),mar=c(2,4,2,2))
for (k in 1:27){
  for (i in ((k-1)*5+1):((k-1)*5+5)){
    plot.ts(prices_ts[,i],col="deepskyblue2",main=colnames(prices_ts[,i]))
    plot.ts(ret_ts[,i],col="deepskyblue2",main=colnames(ret_ts[,i]))
  }
}
plot.ts(prices_ts[,136],col="deepskyblue2",main=colnames(prices_ts[,136]))
plot.ts(ret_ts[,136],col="deepskyblue2",main=colnames(ret_ts[,136]))
plot.ts(prices_ts[,137],col="deepskyblue2",main=colnames(prices_ts[,137]))
plot.ts(ret_ts[,137],col="deepskyblue2",main=colnames(ret_ts[,137]))
plot.ts(prices_ts[,138],col="deepskyblue2",main=colnames(prices_ts[,138]))
plot.ts(ret_ts[,138],col="deepskyblue2",main=colnames(ret_ts[,138]))

#########################################################################################################
# The following returns contain strange data point: XST.USD, IOC.USD and XAS.USD
#########################################################################################################

# XST.USD has a data that appears to be mistaken, so we corrected

which(colnames(ret_ts)=="XST.USD")
par(mfrow=c(1,2))
plot.ts(prices_ts[,111],col="deepskyblue2",main=colnames(prices_ts[,111]))
plot.ts(ret_ts[,111],col="deepskyblue2",main=colnames(ret_ts[,111]))

which(prices_ts[,111]>4)
prices_ts[472,111] <- NA
prices_ts[472,111] <- na_interpolation(prices_ts[,111],option="stine")[472]
ret_ts[,111] <- Delt(prices_ts[,111],type="log")[-1]

# IOC.USD has a data that appears to be mistaken, so we corrected

which(colnames(ret_ts)=="IOC.USD")
par(mfrow=c(1,2))
plot.ts(prices_ts[,118],col="deepskyblue2",main=colnames(prices_ts[,118]))
plot.ts(ret_ts[,118],col="deepskyblue2",main=colnames(ret_ts[,118]))

which(prices_ts[501:1000,118]>3) + 500
prices_ts[872,118] <- NA
prices_ts[872,118] <- na_interpolation(prices_ts[,118],option="stine")[872]
ret_ts[,118] <- Delt(prices_ts[,118],type="log")[-1]

# XAS.USD has a very strange behavior at the end, so it is better to skip it from the analysis

which(colnames(ret_ts)=="XAS.USD")
par(mfrow=c(1,2))
plot.ts(prices_ts[,121],col="deepskyblue2",main=colnames(prices_ts[,121]))
plot.ts(ret_ts[,121],col="deepskyblue2",main=colnames(ret_ts[,121]))

prices_ts <- prices_ts[,-121]
dim(prices_ts)
ret_ts <- ret_ts[,-121]
dim(ret_ts)

#########################################################################################################
# There are 137 return series. Let's plot them and see problems
#########################################################################################################

par(mfrow=c(5,2),mar=c(2,4,2,2))
for (k in 1:27){
  for (i in ((k-1)*5+1):((k-1)*5+5)){
    plot.ts(prices_ts[,i],col="deepskyblue2",main=colnames(prices_ts[,i]))
    plot.ts(ret_ts[,i],col="deepskyblue2",main=colnames(ret_ts[,i]))
  }
}
plot.ts(prices_ts[,136],col="deepskyblue2",main=colnames(prices_ts[,136]))
plot.ts(ret_ts[,136],col="deepskyblue2",main=colnames(ret_ts[,136]))
plot.ts(prices_ts[,137],col="deepskyblue2",main=colnames(prices_ts[,137]))
plot.ts(ret_ts[,137],col="deepskyblue2",main=colnames(ret_ts[,137]))

#########################################################################################################
# Get skewness and kurtosis of returns and plot them

skew_ret_ts <- skewness(ret_ts)
kurt_ret_ts <- kurtosis(ret_ts)

sk_ret_ts <- cbind(skew_ret_ts,kurt_ret_ts)
colnames(sk_ret_ts) <- c("Skewness","Kurtosis")
head(sk_ret_ts)

par(mfrow=c(1,1))
plot(sk_ret_ts,pch=19,col="steelblue2",main="Skewness-Kurtosis plot")

#########################################################################################################
# There are some return series with large extremes creating large skewness and kurtosis
# These extremes are those that creates also large volatilities

#########################################################################################################
# Let's save the objects prices_ts and ret_ts

rm(list= ls()[!(ls() %in% c('prices_ts','ret_ts'))])
save.image("G:\\Mi unidad\\MSDS\\TFM\\ret_end_ts.RData")

#########################################################################################################
# To load the workspace use

load("G:\\Mi unidad\\MSDS\\TFM\\ret_end_ts.RData")
