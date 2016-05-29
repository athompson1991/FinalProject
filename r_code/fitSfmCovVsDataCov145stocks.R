rm(list=ls())
library(factorAnalytics)

# INPORT 145 STOCKS 6 SCORES AS DATA FRAME STACKED BY MONTHS
# AND OPTIONALLY GET SHORT FIVE YEAR SEGMENT INSTEAD OF 25 YEARS
data = "stocks145scores6.csv"
stacked.df <- read.table(file=data,header=TRUE,sep=",",as.is=TRUE)

short = T
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
                          stacked.df$DATE <= as.yearmon("2012-12-31"),]}

# CONVERT RETURNS IN STACKED.DF TO A MULTIVARITE XTS OBJECT
assetRet = tapply(stacked.df$RETURN,list(stacked.df$DATE,stacked.df$TICKER),I)
ret = xts(assetRet,as.yearmon(rownames(assetRet)))
dim(ret)

fit.pca = fitSfm(ret, k = 2)
fit.pca$k

# COMPUTE COVARIANCE MATRIX EIGENVALUES
retmat = coredata(ret)
covret = cov(retmat)
evsret = eigen(covret,symmetric=T,only.values=T)$values
barplot(evsret[1:10],main = "Cov Ret EV's")
covfacmod = fit.pca$Omega
evsfacmod = eigen(covfacmod,symmetric=T,only.values=T)$values
barplot(evsfacmod[1:10],main = "Cov Facmod EV's")

barplot(evsret[131:145],main = "Cov Ret EV's")
barplot(evsfacmod[131:145],main = "Cov Facmod EV's")


