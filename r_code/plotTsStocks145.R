rm(list=ls())
library(factorAnalytics)

# INPORT 145 STOCKS 6 SCORES AS DATA FRAME STACKED BY MONTHS
data = "stocks145scores6.csv"
stacked.df <- read.table(file=data,header=TRUE,sep=",",as.is=TRUE)
class(stacked.df$DATE)
tail(stacked.df$DATE)

# OPTIONALLY GET FIVE YEAR SEGMENT
short = F
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
                          stacked.df$DATE <= as.yearmon("2012-12-31"),]}

# CONVERT RETURNS IN STACKED.DF TO A MULTIVARITE XTS OBJECT
assetRet = tapply(stacked.df$RETURN,list(stacked.df$DATE,stacked.df$TICKER),I)
ret = xts(assetRet,as.yearmon(rownames(assetRet)))

dim(ret)

# PLOT TIMES SERIES OF STOCK RETURNS
plot.ts = function(ret) {
  groupSize = 10
  groups = length(colnames(ret))/groupSize
  for (i in 1:groups) {
    start <- (i-1)*groupSize + 1
    end <- start + groupSize - 1
    plot.zoo(ret[,start:end], main = paste("Group",i))
  }
  start = end+1
  end = dim(ret)[2]
  endGroup = ceiling(groups)+
  if(end >= start)
    {last = ceiling(groups)
     plot.zoo(ret[,start:end],main = paste("Group",last))}
}
plot.ts(ret)

# COMPUTE COVARIANCE MATRIX EIGENVALUES
retmat = coredata(ret)
covdat = cov(ret)
evs = eigen(covdat,symmetric=T,only.values=T)$values
n = 145
par(mfrow=c(1,1))
barplot(evs[1:n])

