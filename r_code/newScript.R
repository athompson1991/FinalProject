library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(lattice)


returns = smallcapW    # Weekly returns 1997 throug 2010
MARKET = returns[,"Weekvwretd"]
returns = smallcapW[,1:20]

initial_interval = c(-0.2, 0.2)
added_size = seq(-0.3, 0.0, length.out = 10)
table_list <- list()

for(i in added_size){
  list_entry_nm = paste(initial_interval[1] + i, initial_interval[2] + i, sep = "_")
  funds = colnames(returns)
  pspec = portfolio.spec(assets=funds)
  pspec.fi = add.constraint(pspec, type="full_investment")
  pspec.box = add.constraint(pspec.fi,type="box",min=initial_interval[1] + i,max=initial_interval[2])
  pspec.gmvBox = add.objective(pspec.box, type="risk", name="var")

  bt.gmvBox <- optimize.portfolio.rebalancing(returns, pspec.gmvBox,
                                              optimize_method="quadprog",
                                              rebalance_on="weeks",
                                              training_period=104,
                                              rolling_window=104)

  wts.gmvBox = extractWeights(bt.gmvBox)

  GMV.BOX = Return.rebalancing(returns,wts.gmvBox)
  print(list_entry_nm)

  table_list[[list_entry_nm]] <- SharpeRatio.annualized(GMV.BOX)

}

