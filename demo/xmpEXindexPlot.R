
#
# Example:
#	Extremal Index Plot
#
# Description:
#  	Plot the extremal as obtained from the 
#	BMW stock and NYSE  Composite Index data.
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

  	par(mfrow = c(3, 2), cex = 0.6)
  	data(bmwres)
  	data(nyseres)
   	
# Investigate BMW log-Returns:

  	x = bmwres[, 1]
	exindexPlot( x, block = 63, autoscale = FALSE, ylim = c(0, 1.2))
	exindexPlot(-x, block = 63, autoscale = FALSE, ylim = c(0, 1.2))

# Investigate NYSE log-Returns:

  	x = nyseres[, 1]
   	exindexPlot( x, block = 50, autoscale = FALSE, ylim = c(0, 1.2))
  	exindexPlot(-x, block = 50, autoscale = FALSE, ylim = c(0, 1.2))
  	
# Reverse Plottype:

   	exindexPlot( x, block = 50, plottype = "K", autoscale = FALSE, 
   		ylim = c(0, 1.2))
  	exindexPlot(-x, block = 50, plottype = "K", autoscale = FALSE, 
  		ylim = c(0, 1.2))

  	