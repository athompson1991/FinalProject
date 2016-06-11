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
pspec.box = add.constraint(pspec,type="box",min=-.03,max=0.025)

pspec.gmvBox = add.objective(pspec.fi, type="risk", name="var")


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
# gmv_performance_chart <- charts.PerformanceSummary(ret.comb,wealth.index = T,
#           lty = c(1,3,2), colorset = c("black","blue","red"),
#           cex.legend = 1.3,cex.axis = 1.3)

metric_list <- c( "ES"
                 ,"maxDrawdown"
                 ,"Return.annualized"
                 ,"Return.cumulative"
                 ,"SortinoRatio"
                 ,"StdDev.annualized")
arg.list <- list(
   ES = list(method = "historical")
  ,maxDrawdown = list()
  ,Return.annualized =  list()
  ,Return.cumulative = list()
  ,SortinoRatio = list()
  ,StdDev.annualized = list()
)
gmv_perf_table <- table.Performance(ret.comb, metrics = metric_list, interactive = F, arg.list = arg.list)$resultingtable
gmv_perf_table <- rbind( gmv_perf_table
                        ,SharpeRatio.annualized = SharpeRatio.annualized(ret.comb)
                        ,SharpeRatio = SharpeRatio(ret.comb, FUN = "StdDev")
                        ,STARR = SharpeRatio(ret.comb, FUN = "ES")
                        )


# Shorting analysis.

min_range <- seq(-0.01, 0.0, length.out = 10)
short_tables <- list()
i = 1

pspec.short = add.constraint(pspec.fi,type="box",min = -1,max=0.07)
pspec.gmvBoxShort = add.objective(pspec.short, type="risk", name="var")
bt.gmvBoxShort <- optimize.portfolio.rebalancing(returns, pspec.gmvBoxShort
                                                 ,optimize_method = "quadprog"
                                                 ,rebalance_on = "weeks"
                                                 ,training_period = 104
                                                 ,rolling_window = 104

)
wts.gmvBoxSh = extractWeights(bt.gmvBoxShort)
GMV.SHORT = Return.rebalancing(returns, wts.gmvBoxSh)
ret_boxes <- na.omit(merge(GMV.BOX, GMV.SHORT, MARKET))
names(ret_boxes) <- c("GMV.BOX", "GMV.SHORT", "MARKET")
short_perf_table <- table.Performance(ret_boxes, metrics = metric_list, interactive = F, arg.list = arg.list)$resultingtable
short_perf_table <- rbind( short_perf_table
                         ,SharpeRatio.annualized = SharpeRatio.annualized(ret_boxes)
                         ,SharpeRatio = SharpeRatio(ret_boxes, FUN = "StdDev")
                         ,STARR = SharpeRatio(ret_boxes, FUN = "ES")
)



# Calculte the DIV values for the time series of the weights
DIV.GMV.LO=DIV(wts.gmvLo)
DIV.GMV.BOX=DIV(wts.gmvBox)
DIV.comb=na.omit(merge(DIV.GMV.LO,DIV.GMV.BOX,all=F))
div_plot <- xyplot(DIV.comb,scales=list(y="same"),main="Diversification")
div_tbl  <- apply(DIV.comb, 2, mean)
ADIV.comb=sapply(DIV.comb,mean)

# Calculate the TO values for the time series of weights
TO.GMV.LO=TO(wts.gmvLo)
TO.GMV.BOX=TO(wts.gmvBox)

TO.comb=na.omit(merge(TO.GMV.LO,TO.GMV.BOX,all=F))
to_plot <- xyplot(TO.comb,scales=list(y="same"),main="Turnover")
to_tbl  <- apply(TO.comb, 2, mean)
ATO.comb=sapply(TO.comb,mean)

to_div_tbl <- rbind(
  DIV = div_tbl
  ,TO = to_tbl
)
