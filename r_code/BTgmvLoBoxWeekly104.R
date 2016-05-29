library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(lattice)
returns = smallcapW    # Weekly returns 1997 throug 2010
MARKET = returns[,"Weekvwretd"]
returns = smallcapW[,1:20]
# plot.zoo(returns, main = "SMALL CAPS")

funds = colnames(returns)
pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec, type="full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.gmvLo = add.objective(pspec.lo, type="risk", name="var")
pspec.box = add.constraint(pspec.fi,type="box",min=0,max=.2)
pspec.gmvBox = add.objective(pspec.box, type="risk", name="var")


# Optimize Portfolio at Monthly Rebalancing and 5-Year Training

bt.gmvLo <- optimize.portfolio.rebalancing(returns, pspec.gmvLo,
                                           optimize_method="quadprog",
                                           rebalance_on="weeks",
                                           training_period=104,
                                           rolling_window=104)
bt.gmvBox <- optimize.portfolio.rebalancing(returns, pspec.gmvBox,
                                            optimize_method="quadprog",
                                            rebalance_on="weeks",
                                            training_period=104,
                                            rolling_window=104)

# Extract time series of portfolio weights
wts.gmvLo = extractWeights(bt.gmvLo)
wts.gmvBox = extractWeights(bt.gmvBox)

# Compute cumulative returns of portfolio
GMV.LO = Return.rebalancing(returns, wts.gmvLo)
GMV.BOX = Return.rebalancing(returns,wts.gmvBox)

# Combine GMV.LO and MARKET cumulative return0
ret.comb <- na.omit(merge(GMV.LO, GMV.BOX, MARKET, all=F))
names(ret.comb) = c("GMV.LO","GMV.BOX","MARKET")

# return analysis
charts.PerformanceSummary(ret.comb,wealth.index = T,
        lty = c(1,3,2), colorset = c("black","blue","red"),
        cex.legend = 1.3,cex.axis = 1.3)

# Calculte the DIV values for the time series of the weights
DIV.GMV.LO=DIV(wts.gmvLo)
DIV.GMV.BOX=DIV(wts.gmvBox)
DIV.comb=na.omit(merge(DIV.GMV.LO,DIV.GMV.BOX,all=F))
xyplot(DIV.comb,scales=list(y="same"),main="The DIV values GMV.LO and GMV.BOX")
ADIV.comb=sapply(DIV.comb,mean,2)
# Print average diversification values

# Calculate the TO values for the time series of weights
TO.GMV.LO=TO(wts.gmvLo)
TO.GMV.BOX=TO(wts.gmvBox)

TO.comb=na.omit(merge(TO.GMV.LO,TO.GMV.BOX,all=F))
xyplot(TO.comb,scales=list(y="same"),main="The TO values of GMV.LO and GMV.VOX")
ATO.comb=sapply(TO.comb,mean,2)
