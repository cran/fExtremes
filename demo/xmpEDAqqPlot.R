
#
# Example:
#   Quantile-Quantile Plots
#
# Description:
#  	Create QQ-plots of the logarithmic returns 
#	of the NYSE Composite Index and of the BMW 
#	share prices versus the standard normal 
#	distribution function.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	par(mfrow = c(2, 2), cex = 0.6)
	data(nyseres)
	data(bmwres)
	
# Plot Time Series:

	ts.plot(as.ts(nyseres), ylab = "Log Returns", 
		main = "NYSE Daily Log Returns")
	ts.plot(as.ts(bmwres), ylab = "Log Returns", 
		main = "BMW Daily Log Returns")
		
# QQ-Plots:

	qqnorm(as.ts(nyseres), ylab = "NYSE Residuals", ylim = c(-0.22, 0.22), 
		main = "NYSE QQ-Plot")
	qqline(nyseres)
	qqnorm(as.ts(bmwres), ylab = "BMW Residuals", ylim = c(-0.15, 0.15), 
		main = "BMW QQ-Plot")
	qqline(bmwres)
	
