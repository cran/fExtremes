
#
# Example:
#	Hill Plot
#
# Description:
#  	Plot the shape parameter obtained from Hill's 
#	estimator as a function of the order statistic 
#	and threshold values together with its 95% 
#	confidence intervals for the NYSE residuals 
#	in comparison to simulated alpha-stable 
#	residuals with index alpha=1.8.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

	set.seed(471)
	par(mfrow = c(3,2), err= -1)
	data(nyseres)
		
	
# Create alpha-Stable and load NYSE log Returns:

	x1 = rsymstb(10000, alpha = 1.8)
	x2 = nyseres[, 1]
  
	
# Hill Plot with Errors for Upper and Lower Tails:

	result = hillPlot( x1, autoscale = FALSE, ylim = c(0, 3.5)) 
	result = hillPlot( x2, autoscale = FALSE, ylim = c(0, 6.0)) 
	result = hillPlot(-x1, autoscale = FALSE, ylim = c(0, 3.5)) 
	result = hillPlot(-x2, autoscale = FALSE, ylim = c(0, 6.0))

	