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

# COMPUTE COVARIANCE MATRIX EIGENVALUES
retmat = coredata(ret)
covdat = cov(ret)
evs = eigen(covdat,symmetric=T,only.values=T)$values
n = 145
# For 25 years
if(short)
  {barplot(evs[131:n],main = "5 YEARS, 145 STOCKS:   ALL EIGENVALUES'S")
   barplot(evs[59:n],main = "5 YEARS, 145 STOCKS: SMALLEST 86 E.V.'S")}
barplot(evs[1:n],main = "25 YEARS, 145 STOCKS:   ALL EIGENVALUES'S")
barplot(evs[131:n],main = "25 YEARS, 145 STOCKS: SMALLEST 15 E.V.'S")


