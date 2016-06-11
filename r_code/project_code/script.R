source("E:/School Stuff/CFRM 543 - Portfolio Optimization & Asset Management/final_project_git/r_code/project_code/functions.R")


##########
# Part 1 #
##########

# Get data

returns <- smallcapW[,1:20]
MARKET <- returns[,"Weekvwretd"]
funds <- colnames(returns)

# GMV section

port_spec <- portfolio.spec(assets=funds)
port_spec_fi <- add.constraint(port_spec, type="full_investment")
port_spec_lo <- add.constraint(port_spec_fi, type="long_only")
port_spec_box <- add.constraint(port_spec_fi,type="box",min=-0.0,max=0.2)
port_spec_short <- add.constraint(port_spec,type="box",min = -.03,max=0.025)

port_obj_gmvLo <- add.objective(port_spec_lo, type="risk", name="var")
port_obj_gmvBox <- add.objective(port_spec_box, type="risk", name="var")
port_obj_gmvBoxShort <- add.objective(port_spec_short, type="risk", name="var")

specs_lists <- list(
   gmvLo <- port_obj_gmvLo
  ,gmvBox <- port_obj_gmvBox
  # ,gmvShort <- port_obj_gmvBoxShort
)

gmv_calcs_w60 <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "weeks")
gmv_calcs_m60 <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "months")
gmv_calcs_w104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "weeks")
gmv_calcs_m104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "months")


# QU section

perf_ls_w60 <- list()
perf_ls_m60 <- list()
perf_ls_w104 <- list()
perf_ls_m104 <- list()

qu_calc_ls_w60 <- list()
qu_calc_ls_m60 <- list()
qu_calc_ls_w104 <- list()
qu_calc_ls_m104 <- list()

in_seq <- seq(10, 40, by = 5)
i = 1
for(lambda in in_seq){
  # lambda <- 20
  port_obj_quLo <- add.objective(port_spec_lo, type="quadratic_utility", name="var", risk_aversion = lambda)
  port_obj_quBox <- add.objective(port_spec_box, type="quadratic_utility", name="var", risk_aversion = lambda)
  # port_obj_quBoxShort <- add.objective(port_spec_short, type="quadratic_utility", name="var", risk_aversion = lambda)
  
  
  specs_list <- list(
     # gmvLo  = port_obj_gmvLo
    # ,gmvBox = port_obj_gmvBox
    quLo = port_obj_quLo
    ,quBox = port_obj_quBox
    # ,gmvShort = port_obj_gmvBoxShort
    # ,quBoxShort = port_obj_quBoxShort
  )
  
  port_calcs_w60 <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "weeks",opt_method = "quadprog")
  port_calcs_m60 <- do_portfolio_calc(returns, specs_list, roll_window = 60, rebalance = "months")
  port_calcs_w104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "weeks")
  port_calcs_m104 <- do_portfolio_calc(returns, specs_list, roll_window = 104, rebalance = "months")
  perf_ls_w60[[i]] <- extract_table(port_calcs_w60)
  perf_ls_m60[[i]] <- extract_table(port_calcs_m60)
  perf_ls_w104[[i]] <- extract_table(port_calcs_w104)
  perf_ls_m104[[i]] <- extract_table(port_calcs_w104)
  
  qu_calc_ls_w60[[i]] <- port_calcs_w60
  qu_calc_ls_m60[[i]] <- port_calcs_m60
  qu_calc_ls_w104[[i]] <- port_calcs_w104
  qu_calc_ls_m104[[i]] <- port_calcs_m104
  i = i + 1
}

# Minimum Expected Shortfall

p_vals <- c(0.9, 0.95)
es_ls <- lapply(p_vals, function(p){
  port_obj_gmesLo <- add.objective(port_spec_lo, type="risk", name="ES", arguments = list(p=p))
  port_obj_gmesBox <- add.objective(port_spec_box, type="risk", name="ES", arguments = list(p=p))
  port_obj_gmesBoxShort <- add.objective(port_spec_short, type="risk", name="ES", arguments = list(p=p))
  
  gmes_specs_ls <- list(
     gmesLo = port_obj_gmesLo
    ,gmesBox = port_obj_gmesBox
    ,gmesBoxShort = port_obj_gmesBoxShort
  )
  gmes_calcs_w60  <- do_portfolio_calc(returns, gmes_specs_ls, roll_window = 60,  rebalance = "weeks",  opt_method = "glpk")
  gmes_calcs_m60  <- do_portfolio_calc(returns, gmes_specs_ls, roll_window = 60,  rebalance = "months", opt_method = "glpk")
  gmes_calcs_w104 <- do_portfolio_calc(returns, gmes_specs_ls, roll_window = 104, rebalance = "weeks",  opt_method = "glpk")
  gmes_calcs_m104 <- do_portfolio_calc(returns, gmes_specs_ls, roll_window = 104, rebalance = "months", opt_method = "glpk")
  list(gmes_calcs_w60
       ,gmes_calcs_m60
       ,gmes_calcs_w104
       ,gmes_calcs_m104)
})