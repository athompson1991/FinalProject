library(mpo)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(lattice)

custom_table_report <- function(returns){
  metric_list <- c( "ES"
                 ,"maxDrawdown"
                 ,"Return.annualized"
                 ,"Return.cumulative"
                 ,"SortinoRatio"
                 ,"StdDev.annualized")
  arg_list <- list(
     ES = list(method = "historical")
    ,maxDrawdown = list()
    ,Return.annualized =  list()
    ,Return.cumulative = list()
    ,SortinoRatio = list()
    ,StdDev.annualized = list()
  )
  perf_table <- table.Performance(returns, metrics = metric_list, interactive = F, arg.list = arg_list)$resultingtable
  perf_table <- rbind( perf_table
                          ,SharpeRatio.annualized = SharpeRatio.annualized(returns)
                          ,SharpeRatio = SharpeRatio(returns, FUN = "StdDev")
                          ,STARR = SharpeRatio(returns, FUN = "ES")
                          )
  perf_table
}

get_div_and_to <- function(wts_ls){
  port_nms <- names(wts_ls)
  div_vals <- lapply(wts_ls, DIV)
  names(div_vals) <- port_nms
  to_vals <- lapply(wts_ls, TO)
  names(to_vals) <- port_nms
  div_plots <- list()
  to_plots <- list()
  for(i in 1:length(wts_ls)){
    div_plots[[i]] <- xyplot(div_vals[[i]], scales=list(y="same"),main="Diversification")
    to_plots[[i]] <- xyplot(to_vals[[i]],scales=list(y="same"),main="Turnover")
  }
  names(div_plots) <- port_nms; names(to_plots) <- port_nms
  div_means <- sapply(div_vals, mean)
  to_means <- sapply(to_vals, mean)
  stats_df <- data.frame(div = div_means, to = to_means)
  list(div = div_vals, trn_ov = to_vals, div_plots = div_plots, to_plots = to_plots, stats_df = stats_df)
}

do_portfolio_calc <- function(rtn, specs_ls){
  port_names <- names(specs_ls)
  out_ls <- list()
  temp_tables <- list()
  for(i in 1:length(specs_ls)){
    optimization <- optimize.portfolio.rebalancing(rtn, specs_ls[[i]]
                                                   ,optimize_method = "quadprog", rebalance_on = "weeks"
                                                   ,training_period = 104, rolling_window = 104)
    wts <- extractWeights(optimization)
    port_returns <- Return.rebalancing(rtn, wts)
    out_ls[[i]] <- list(opt = optimization, wts = wts, returns = port_returns, perf = custom_table_report(port_returns))
  }
  names(out_ls) <- port_names
  out_ls
}

extract_table <- function(port_calc_ls){
  df_nms <- names(port_calc_ls)
  table_ls <- lapply(port_calc_ls, function(calc) calc[["perf"]])
  table_df <- do.call(cbind, table_ls)
  colnames(table_df) <- df_nms
  table_df
}

MARKET <- returns[,"Weekvwretd"]
returns <- smallcapW[,1:20]


funds = colnames(returns)
pspec = portfolio.spec(assets=funds)
port_spec_fi = add.constraint(pspec, type="full_investment")
port_spec_lo = add.constraint(port_spec_fi, type="long_only")
port_spec_gmvLo = add.objective(port_spec_lo, type="risk", name="var")
port_spec_box = add.constraint(port_spec_fi,type="box",min=-0.0,max=0.2)

port_spec_gmvBox = add.objective(port_spec_box, type="risk", name="var")
pspec_short = add.constraint(port_spec_fi,type="box",min = -1,max=0.07)
port_spec_gmvBoxShort = add.objective(pspec_short, type="risk", name="var")

specs_list <- list(
   gmvLo = port_spec_gmvLo
  ,gmvBox = port_spec_gmvBox
  ,gmvShort = port_spec_gmvBoxShort
)
port_calcs <- do_portfolio_calc(returns, specs_list)
extract_table(port_calcs)
