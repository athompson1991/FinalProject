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

data = "E:/School Stuff/CFRM 543 - Portfolio Optimization & Asset Management/final_project_git/data/stocks145bonds7.csv"
retAll = read.zoo(data,sep=",",header = T,format = "%m/%d/%Y")
ret16yr = retAll[109:300,]
SP500 = as.xts(ret16yr[,157])
ret = ret16yr[,1:145]
dim(ret)

fundNames = names(ret)
pspec = portfolio.spec(assets=fundNames)
pspec.fi = add.constraint(pspec, type="full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.gmvLo = add.objective(pspec.lo, type="risk", name="var")

bt.gmvLoSFM <- optimize.portfolio.rebalancing(ret, pspec.gmvLo,
                                    optimize_method="quadprog",
                                    momentFUN = "custom.portfolio.moments",
                                    rebalance_on="months",
                                    training_period=60,
                                    rolling_window=60)

# Extract time series of portfolio weights
wts.gmvLoSFM = extractWeights(bt.gmvLoSFM)
wts.gmvLoSFM

# 
# # Compute cumulative returns of portfolio
# GMV.LO.SFM = Return.rebalancing(ret,wts.gmvLoSFM)
# class(GMV.LO.SFM)
# class(SP500)
# 
# 
# # Combine GMV.LO and GMV.LO.SFM cumulative return
# ret.comb <- na.omit(merge(SP500,GMV.LO.SFM,all=F))
# names(ret.comb) = c("SP500","GMV.LO.SFM")
# 
# # return analysis
# charts.PerformanceSummary(ret.comb,wealth.index = T,
#         lty = c(3,1), colorset = c("black","blue"),
#         cex.legend = 1.3,cex.axis = 1.3,cex.main = 1.3,
#         main = "145 STOCKS 1999-2014 GMV.LO SFM COVARIANCE vs SP500")
#         
# 
# 
# # Calculte the average DIV value
# DIV.GMV.LO.SFM=DIV(wts.gmvLoSFM)
# mean(DIV.GMV.LO.SFM)
# print(ADIV.comb)
#   
# # Calculate the average TO value
# TO.GMV.LO.SFM=TO(wts.gmvLoSFM)
# mean(TO.GMV.LO.SFM)
