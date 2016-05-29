library(covmat)
library(PortfolioAnalytics)

custom.portfolio.moments <- function(R, portfolio) {
  momentargs <- list()
  momentargs$mu  <-  matrix(as.vector(apply(R,2, "mean")), ncol = 1)
  momentargs$sigma  <-  estRMT(R, parallel=FALSE)$cov
  momentargs$m3 <- matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
  momentargs$m4 <- matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
  
  return(momentargs)
}

data("dow30data")

pspec.lo <- portfolio.spec(assets = colnames(dow30data))

#long-only
pspec.lo <- add.constraint(pspec.lo, type="full_investment")
pspec.lo <- add.constraint(pspec.lo, type="long_only")

pspec.lo <- add.objective(portfolio=pspec.lo, type="return", name="mean")
pspec.lo <- add.objective(portfolio=pspec.lo, type="risk", name="var")

opt.ordinary <- 
  optimize.portfolio.rebalancing(dow30data, pspec.lo, 
                                 optimize_method="quadprog",
                                 rebalance_on="months", 
                                 training_period=120,
                                 trailing_periods=120)
opt.rmt <- 
  optimize.portfolio.rebalancing(dow30data, pspec.lo, 
                                 optimize_method="quadprog",
                                 momentFUN = "custom.portfolio.moments",
                                 rebalance_on="months", 
                                 training_period=120, 
                                 trailing_periods=120)

ordinary.wts <- na.omit(extractWeights(opt.ordinary))
ordinary <- Return.rebalancing(R=dow30data, weights=ordinary.wts)

rmt.wts <- na.omit(extractWeights(opt.rmt))
rmt <- Return.rebalancing(R=dow30data, weights=rmt.wts)

rmt.strat.rets <- merge.zoo(ordinary,rmt)
colnames(rmt.strat.rets) <- c("ordinary", "rmt")

charts.PerformanceSummary(rmt.strat.rets,wealth.index = T, 
                          colorset = c("red","blue"), 
                          main=paste(c("Comparison of Portflio ",
                                       "Performance using two ",
                                       "different covariance matrices"),
                                     collapse=""), cex.legend = 1.3, 
                          cex.axis = 1.3, legend.loc = "topleft")
