
#
# Example:
#	GEV Maximum Likelihood Estimation Fit
#
# Description:
#  	Fit the GEV via the Maximum likelihood approach. 
#	Investigate the data from BMW Stocks
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

  	par(mfrow = c(2, 2), cex = 0.6)
    data(nyseres)
    nyseres = nyseres[,1]
    ts.plot(nyseres, main = "Log Returns")
    mtext("NYSE Index Residuals", line = 0.5, cex = 0.5)
    x = blockMaxima(-nyseres, block = 63)
    mtext("Lower Tail: Quarterly Data", line = 0.5, cex = 0.5)
    
    
# Fit GEV Data by Max Log Likelihood Method a la ISMEV:

   	fit = gevglmFit(x)
    print(fit)
    summary(fit)
    readline("Press any key to continue > ")

    
# Profile Likelihood: 

	gevglmprofxiPlot(fit, xlow = 0.15, xup = 0.60)
	title(main = "Profile Likelihood for Xi")
    gevglmprofPlot(fit, m = 100, xlow = 0.05, xup = 0.15)
    title(main = "Profile Likelihood for Quantile 0.01")
    
    
# Fit GEV Data by Max Log Likelihood Method a la EVIS: 
    
    fit = gevFit(x, type = "mle")
    print(fit)
    summary(fit)
  
     