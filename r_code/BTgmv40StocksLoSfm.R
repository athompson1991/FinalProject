library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(factorAnalytics)
library(lattice)

custom.portfolio.moments = function(R, portfolio) {
  momentargs = list()
  momentargs$mu  =  matrix(as.vector(apply(R,2,"mean")), ncol = 1)
  momentargs$sigma  =  fitSfm(R,k=3)$Omega
  momentargs$m3 = matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
  momentargs$m4 = matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
  return(momentargs)
}

data = "stocks145bonds7.csv"
ret = read.zoo(data,sep=",",header = T,format = "%m/%d/%Y")
ret16yr = ret[109:300,]
ret = ret16yr[,1:40]

funds = names(ret)

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec, type="full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.gmvLo = add.objective(pspec.lo, type="risk", name="var")


# Optimize Portfolio at Monthly Rebalancing and 5-Year Training
bt.gmvLo <- optimize.portfolio.rebalancing(ret, pspec.gmvLo,
                                    optimize_method="quadprog",
                                    rebalance_on="months",
                                    training_period=60,
                                    rolling_window=60)

bt.gmvLoSFM <- optimize.portfolio.rebalancing(ret, pspec.gmvLo,
                                    optimize_method="quadprog",
                                    momentFUN = "custom.portfolio.moments",
                                    rebalance_on="months",
                                    training_period=60,
                                    rolling_window=60)

# Extract time series of portfolio weights
wts.gmvLo = extractWeights(bt.gmvLo)
wts.gmvLoSFM = extractWeights(bt.gmvLoSFM)


# Compute cumulative returns of portfolio
GMV.LO = Return.rebalancing(ret, wts.gmvLo)
GMV.LO.SFM = Return.rebalancing(ret,wts.gmvLoSFM)


# Combine GMV.LO and GMV.LO.SFM cumulative return
ret.comb <- na.omit(merge(GMV.LO,GMV.LO.SFM,all=F))
names(ret.comb) = c("GMV.LO","GMV.LO.SFM")

# return analysis
charts.PerformanceSummary(ret.comb,wealth.index = T,
        lty = c(3,1), colorset = c("black","blue"),
        cex.legend = 1.3,cex.axis = 1.3, cex.main = 1.4,
        main = "40 STOCKS 1999-2014 GMV.LO SAMPLE VS SFM COVARIANCE")
        




# Calculte the DIV values for the time series of the weights
DIV.GMV.LO=DIV(wts.gmvLo)
DIV.GMV.LO.SFM=DIV(wts.gmvLoSFM)
DIV.comb=na.omit(merge(DIV.GMV.LO,DIV.GMV.LO.SFM,all=F))
xyplot(DIV.comb,scales=list(y="same"),main="")
ADIV.comb=sapply(DIV.comb,mean,2)
# Print average diversification values
print(ADIV.comb)
  
# Calculate the TO values for the time series of weights
TO.GMV.LO=TO(wts.gmvLo)
TO.GMV.LO.SFM=TO(wts.gmvLoSFM)

TO.comb=na.omit(merge(TO.GMV.LO,TO.GMV.LO.SFM,all=F))
xyplot(TO.comb,scales=list(y="same"),main="")
ATO.comb=sapply(TO.comb,mean,2)
# Print average turnover values
print(ATO.comb)
