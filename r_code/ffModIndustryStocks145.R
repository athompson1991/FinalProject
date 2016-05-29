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
unique(stacked.df$SECTOR)

# GET FIVE YEAR SEGMENT
short = F
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
	                      stacked.df$DATE <= as.yearmon("2012-12-31"),]}

head(stacked.df$DATE)
tail(stacked.df$DATE)


# FIT FUNDFACMOD
industry.mod <- fitFfm(data = stacked.df, 
	exposure.vars = c("SECTOR"),
	date.var = "DATE", 
  ret.var = "RETURN", 
  asset.var = "TICKER", 
  fit.method="WLS")

names(industry.mod$SECTOR)

barplot(industry.mod$r2,las=2,col=5,
        names.arg= as.yearmon(names(industry.mod$r2)),
        cex.names=0.5,
        main="R-squared Values for Pure Industry Factor Model")

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

# PLOT TIME SERIES OF RESIDUALS FOR 2 x 5 x 5 = 50 STOCKS
residRet = industry.mod$residuals
yname = "RESIDUAL RETURNS (%)"
tsPlotMP(residRet,layout = c(2,5,5),yname = yname, scaleType = "same")  # Now try scaleType = "free" and see which is most useful


# CONVERT 145 STOCK TO AM XTS OBJECT AND PLOT
assetRet = tapply(stacked.df$RETURN,list(stacked.df$DATE,stacked.df$TICKER),I)
assetRet = xts(assetRet,as.yearmon(rownames(assetRet)))

# PLOT TIMES SERIES OF STOCK RETURNS FOR 2 x 5 x 5 = 50 STOCKS
yname = "ASSET RETURNS (%)"
tsPlotMP(assetRet,layout = c(2,5,5),yname = yname, scaleType = "same")  # Now try "free" instead of "same".  What do you think?





