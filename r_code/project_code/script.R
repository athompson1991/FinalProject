source("E:/School Stuff/CFRM 543 - Portfolio Optimization & Asset Management/final_project_git/r_code/project_code/functions.R")

library(doParallel)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
##########
# Part 1 #
##########

# Get data

returns <- smallcapW[,1:20]
MARKET  <- returns[,"Weekvwretd"]
funds   <- colnames(returns)

# GMV section
funds_wts        <- rep(0, length(funds))
names(funds_wts) <- funds
port1_spec       <- portfolio.spec(assets=funds_wts)
port1_spec_fi    <- add.constraint(port1_spec,    type="full_investment")
port1_spec_lo    <- add.constraint(port1_spec_fi, type="long_only")
port1_spec_box   <- add.constraint(port1_spec_fi, type="box",min=-0.0,max=0.2)
port1_spec_short <- add.constraint(port1_spec_fi, type="box",min = -.03,max=0.25)

port1_obj_gmvLo       <- add.objective(port1_spec_lo,    type="risk", name="var")
port1_obj_gmvBox      <- add.objective(port1_spec_box,   type="risk", name="var")
port1_obj_gmvBoxShort <- add.objective(port1_spec_short, type="risk", name="var")

specs_list <- list(
   gmv1Lo    = port1_obj_gmvLo
  ,gmv1Box   = port1_obj_gmvBox
  ,gmv1Short = port1_obj_gmvBoxShort
)

gmv1_calcs_w60  <- do_portfolio_calc(returns, specs_list, roll_window = 60,  rebalance = "weeks")
gmv1_calcs_m60  <- do_portfolio_calc(returns, specs_list, roll_window = 60,  rebalance = "months")
gmv1_calcs_w104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "weeks")
gmv1_calcs_m104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "months")

gmv1_ls <- list(  
                  gmv1_calcs_w60 = gmv1_calcs_w60
                , gmv1_calcs_m60 = gmv1_calcs_m60
                , gmv1_calcs_w104 = gmv1_calcs_w104
                , gmv1_calcs_m104 = gmv1_calcs_m104
                )

# QU section

perf_ls_w60  <- list()
perf_ls_m60  <- list()
perf_ls_w104 <- list()
perf_ls_m104 <- list()

qu1_calc_ls_w60  <- list()
qu1_calc_ls_m60  <- list()
qu1_calc_ls_w104 <- list()
qu1_calc_ls_m104 <- list()

in_seq <- seq(10, 40, by = 5)
i = 1
for(lambda in in_seq){
  # lambda <- 20
  port1_obj_quLo       <- add.objective(port1_spec_lo,  type="quadratic_utility", name="var", risk_aversion = lambda)
  port1_obj_quBox      <- add.objective(port1_spec_box, type="quadratic_utility", name="var", risk_aversion = lambda)
  port1_obj_quBoxShort <- add.objective(port1_spec_short, type="quadratic_utility", name="var", risk_aversion = lambda)
  
  specs_list <- list(
     quLo       = port1_obj_quLo
    ,quBox      = port1_obj_quBox
    ,quBoxShort = port1_obj_quBoxShort
  )
  
  port1_calcs_w60  <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "weeks")
  port1_calcs_m60  <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "months")
  port1_calcs_w104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "weeks")
  port1_calcs_m104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "months")
  
  perf_ls_w60[[i]]  <- extract_table(port1_calcs_w60)
  perf_ls_m60[[i]]  <- extract_table(port1_calcs_m60)
  perf_ls_w104[[i]] <- extract_table(port1_calcs_w104)
  perf_ls_m104[[i]] <- extract_table(port1_calcs_w104)
  
  qu1_calc_ls_w60[[i]]  <- port1_calcs_w60
  qu1_calc_ls_m60[[i]]  <- port1_calcs_m60
  qu1_calc_ls_w104[[i]] <- port1_calcs_w104
  qu1_calc_ls_m104[[i]] <- port1_calcs_m104
  
  i = i + 1
}
names(qu1_calc_ls_w60)  <- paste("lambda", in_seq, sep = "_")
names(qu1_calc_ls_m60)  <- paste("lambda", in_seq, sep = "_")
names(qu1_calc_ls_w104) <- paste("lambda", in_seq, sep = "_")
names(qu1_calc_ls_m104) <- paste("lambda", in_seq, sep = "_")

qu1_ls <- list( 
                  qu1_calc_ls_w60  = qu1_calc_ls_w60
                 ,qu1_calc_ls_m60  = qu1_calc_ls_m60
                 ,qu1_calc_ls_w104 = qu1_calc_ls_w104
                 ,qu1_calc_ls_m104 = qu1_calc_ls_m104
                 )

# Minimum Expected Shortfall

p_vals <- c(0.9, 0.95)
es1_ls <- lapply(p_vals, function(p){
  port1_obj_gmesLo       <- add.objective(port1_spec_lo   , type="risk", name="ES", arguments = list(p=0.9))
  port1_obj_gmesBox      <- add.objective(port1_spec_box  , type="risk", name="ES", arguments = list(p=0.9))
  port1_obj_gmesBoxShort <- add.objective(port1_spec_short, type="risk", name="ES", arguments = list(p=0.9))
  
  gmes1_spec_ls <- list(
     gmes1Lo       = port1_obj_gmesLo
    ,gmes1Box      = port1_obj_gmesBox
    ,gmes1BoxShort = port1_obj_gmesBoxShort
  )
  gmes1_calcs_w60  <- do_portfolio_calc(returns, gmes1_spec_ls, roll_window = 60,  rebalance = "weeks",  opt_method = rep("glpk", 3))
  gmes1_calcs_m60  <- do_portfolio_calc(returns, gmes1_spec_ls, roll_window = 60,  rebalance = "months", opt_method = rep("glpk", 3))
  gmes1_calcs_w104 <- do_portfolio_calc(returns, gmes1_spec_ls, roll_window = 104, rebalance = "weeks",  opt_method = rep("glpk", 3))
  gmes1_calcs_m104 <- do_portfolio_calc(returns, gmes1_spec_ls, roll_window = 104, rebalance = "months", opt_method = rep("glpk", 3))
  
  list(
        gmes1_calcs_w60
       ,gmes1_calcs_m60
       ,gmes1_calcs_w104
       ,gmes1_calcs_m104
      )
})




##########
# Part 2 #
##########

# Get data

stock_data <- read.csv("E:/School Stuff/CFRM 543 - Portfolio Optimization & Asset Management/final_project_git/data/stocks145bonds7.csv")
drop_cols <- seq(ncol(stock_data) - 12, ncol(stock_data))
stocks_xts <- xts(stock_data[ ,-c(1, drop_cols)], order.by = as.Date(stock_data$DATE, "%m/%d/%Y"))

# Single factor model

fit_pca <- fitSfm(stocks_xts, k = 2)

# Global minimum variance

stocks_subset <- stocks_xts[ ,1:144]
funds <- colnames(stocks_subset)
port2_spec            <- portfolio.spec(assets=funds)
port2_spec_fi         <- add.constraint(port2_spec,    type="full_investment")
port2_spec_lo         <- add.constraint(port2_spec_fi, type="long_only")
port2_spec_box        <- add.constraint(port2_spec_fi, type="box",min=-0.0,max=0.5)
port2_spec_short      <- add.constraint(port2_spec,    type="box",min=-0.03,max=0.25)

port2_obj_gmv         <- add.objective(port2_spec_fi,    type="risk", name="var")
port2_obj_gmvLo       <- add.objective(port2_spec_lo,    type="risk", name="var")
port2_obj_gmvBox      <- add.objective(port2_spec_box,   type="risk", name="var")
port2_obj_gmvBoxShort <- add.objective(port2_spec_short, type="risk", name="var")

port2_obj_quLo  <- add.objective(port2_spec_lo,  type="quadratic_utility", name="var", risk_aversion = 20)
port2_obj_quBox <- add.objective(port2_spec_box, type="quadratic_utility", name="var", risk_aversion = 20)

gmv2_specs_list <- list(
   gmv2Lo   = port2_obj_gmvLo
  ,gmv2Box  = port2_obj_gmvBox
  ,gmvShort = port2_obj_gmvBoxShort
)



gmv2_calcs_m200 <- do_portfolio_calc(stocks_subset, gmv2_specs_list, roll_window = 200, rebalance = "months")

qu2_specs_list <- list(
   qu2Lo  = port2_obj_quLo
  ,qu2Box = port2_obj_quBox
)

qu2_calcs_m200 <- do_portfolio_calc(stocks_xts, qu2_specs_list, roll_window = 200, rebalance = "months")

master_ls <- list(
   gmv1 = gmv1_ls
  ,qu1  = qu1_ls
  ,es1  = es1_ls
  ,gmv2 = gmv2_calcs_m200
  ,qu2  = qu2_calcs_m200
)

stopCluster(cl)
