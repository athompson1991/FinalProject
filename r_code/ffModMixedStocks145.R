rm(list=ls())
library(factorAnalytics)
library(lattice)

tsPlotMP = function(ret,add.grid = F,cex = 1.0, layout = NULL,type = "l",
                    pct = 100, yname = "RETURNS (%)",scaleType = "free",
                    lwd = 1, color = "black")
{
  if(add.grid) {type = c("l","g")} else
  {type = type}
  xyplot(pct*ret,par.strip.text = list(cex = cex),type = type,
         xlab="", ylab = list(label = yname,cex = cex), lwd = lwd,
         scales = list(y = list(cex = cex,relation=scaleType),
                       x = list(cex = cex)),layout = layout,
         col = color, strip = F, strip.left = T) 
}

# INDUSTRY FUNDAMENTAL FACTOR MODEL
# INPORT DATA AS DATA FRAME STACKED BY MONTHS
data = "stocks145scores6.csv"
stacked.df <- read.table(file=data,header=TRUE,
                         sep=",",as.is=TRUE)

# GET FIVE YEAR SEGMENT
short = F
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
	                      stacked.df$DATE <= as.yearmon("2012-12-31"),]}
names(stacked.df)

# FIT FUNDFACMOD
industry.mod <- fitFfm(data = stacked.df, 
	exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
	date.var = "DATE", 
  ret.var = "RETURN", 
  asset.var = "TICKER", 
  fit.method="WLS")

barplot(industry.mod$r2,las=2,col=5,
        names.arg= as.yearmon(names(industry.mod$r2)),
        cex.names=0.5,
        main="R-squared Values for Mixed Model")

# TIME SERIES AVERAGE OF R-SQUARED VALUES
mean(industry.mod$r2)

# PLOT TIME SERIES OF FACTOR RETURNS
facRet = industry.mod$factor.returns
tsPlotMP(facRet,yname = "FACTOR RETURNS (%)",scaleType = "same")

# FACTOR RETURNS ROBUST AND CLASSICAL CORRELATIONS AND ELLIPSES
library(robust)
dat = facRet
control = covRob.control("mcd", quan = 0.95)
(corrRob = covRob(dat,estim = "mcd", corr = T, control = control))
(corrClassic = covClassic(dat,corr = T))
(covBoth = fit.models(corrRob,corrClassic))
plot(covBoth,which = 4)
