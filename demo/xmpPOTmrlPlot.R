
#
# Example:
#	POT Mean Residual Life Plot
#
# Description:
#	Create a mean residual life plot. Include
#	approximate confidence intervals, by
#  	default 95%
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Settings:

 	par(mfrow = c(2, 2), cex = 0.6)
 	data(bmwres)
 	data(nyseres)
 	
# Mean Residual Life Plot - BMW Data: 

	bmwres = bmwres[, 1]
	mrlPlot(-bmwres)
	mtext("BMW Losses", line = 0.5, cex = 0.5)
	
# Mean Residual Life Plot - BMW Data: 

	nyseres = nyseres[, 1]
	mrlPlot(-nyseres)
	mtext("NYSE Losses", line = 0.5, cex = 0.5)

	