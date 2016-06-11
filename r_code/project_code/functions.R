library(mpo)
library(PortfolioAnalytics)
library(factorAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(lattice)


custom_table_report <- function(returns){
  perf <- c(
     cumulative_returns = Return.cumulative(returns)
    ,annualized_returns_geometric = Return.annualized(returns)
    ,max_drawdown = maxDrawdown(returns)
    ,annualized_returns_nongeometric = Return.annualized(returns, geometric = F)
    ,annualized_standard_deviation = StdDev.annualized(returns)
    ,annualized_sharpe_ratio = sharpeRatio(returns, annualize = T)
    ,sharpe_ratio = sharpeRatio(returns, annualize = F)
    ,sortino_ratio = SortinoRatio(returns)
    ,starr_ratio = starrRatio(returns)
    ,expected_tail_loss = etl(returns)
  )
  perf
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

do_portfolio_calc <- function(returns, specs_ls, roll_window = 104, rebalance = "weeks",opt_method = rep("quadprog",length(specs_ls)), ...){
  port_names <- names(specs_ls)
  out_ls <- list()
  temp_tables <- list()
  for(i in 1:length(specs_ls)){
    print(port_names[i])
    optimization <- optimize.portfolio.rebalancing(returns, specs_ls[[i]]
                                                   ,optimize_method = opt_method[i], rebalance_on = rebalance
                                                   ,training_period = roll_window, rolling_window = roll_window, ...)
    wts <- extractWeights(optimization)
    port_returns <- Return.portfolio(returns, wts)
    print(opt_method)
    print(head(port_returns))
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

calc_ls_chart <- function(port_calc_ls){
  returns_ls <- lapply(port_calc_ls, function(x) x[["returns"]])
  returns_df <- do.call(merge, returns_ls)
  colnames(returns_df) <- names(port_calc_ls)
  charts.PerformanceSummary(returns_df,wealth.index = T,
                            cex.legend = 1.3,cex.axis = 1.3, cex.main = 1.4)

}

custom_portfolio_moments = function(R, portfolio) {
  momentargs = list()
  momentargs$mu  =  matrix(as.vector(apply(R,2,"mean")), ncol = 1)
  momentargs$sigma  =  fitSfm(R,k=3)$Omega
  momentargs$m3 = matrix(0, nrow=ncol(R), ncol=ncol(R)^2)
  momentargs$m4 = matrix(0, nrow=ncol(R), ncol=ncol(R)^3)
  return(momentargs)
}

